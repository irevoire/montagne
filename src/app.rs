use egui::PointerButton;
use egui_plot::{Line, MarkerShape, Plot, PlotPoints, Points};
use itertools::Itertools;

const DELIM: &str = "#-----------\n";

#[derive(Default, serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct App {
    input: String,
    snapshots: Vec<Snapshot>,
    opened_snapshots: Vec<usize>,
    plot: bool,
}

impl App {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        if let Some(storage) = cc.storage {
            eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default()
        } else {
            Default::default()
        }
    }
}

impl eframe::App for App {
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::MenuBar::new().ui(ui, |ui| {
                egui::widgets::global_theme_preference_buttons(ui);
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            if !self.plot {
                if ui.button("plot").clicked() {
                    self.plot = true;
                    self.snapshots = parse(&self.input);
                }
                ui.label("Paste your massif file here: ");
                egui::ScrollArea::vertical().show(ui, |ui| {
                    ui.text_edit_multiline(&mut self.input);
                });
            } else {
                if ui.button("reset").clicked() {
                    self.plot = false;
                    self.opened_snapshots.clear();
                }
                let points: PlotPoints<'_> = self
                    .snapshots
                    .iter()
                    .map(|snap| [snap.id as f64, snap.mem_heap_b as f64])
                    .collect();
                let peaks: PlotPoints<'_> = self
                    .snapshots
                    .iter()
                    .filter(|snap| matches!(snap.heap_tree, HeapTree::Peak(_)))
                    .map(|snap| [snap.id as f64, snap.mem_heap_b as f64])
                    .collect();
                let peaks = Points::new("peak", peaks)
                    .shape(MarkerShape::Diamond)
                    .radius(10.0);
                let detailed: PlotPoints<'_> = self
                    .snapshots
                    .iter()
                    .filter(|snap| matches!(snap.heap_tree, HeapTree::Detailed(_)))
                    .map(|snap| [snap.id as f64, snap.mem_heap_b as f64])
                    .collect();
                let detailed = Points::new("detailed", detailed)
                    .shape(MarkerShape::Cross)
                    .radius(10.0);
                let line = Line::new("mem_heap_b", points);
                Plot::new("my_plot").view_aspect(2.0).show(ui, |plot_ui| {
                    if plot_ui.response().clicked_by(PointerButton::Primary)
                        && let Some(pos) = plot_ui.pointer_coordinate()
                    {
                        let pos = self
                            .snapshots
                            .iter()
                            .position_min_by_key(|snap| snap.id.abs_diff(pos.x as usize))
                            .unwrap();
                        if self.snapshots[pos].heap_tree != HeapTree::Empty {
                            self.opened_snapshots.push(pos);
                        }
                    }
                    plot_ui.line(line);
                    plot_ui.points(peaks);
                    plot_ui.points(detailed);
                });
            }
        });
        let mut remove_pos = Vec::new();
        for (pos, opened_snapshots) in self.opened_snapshots.iter().enumerate() {
            let snap = &self.snapshots[*opened_snapshots];
            let suffix = if matches!(snap.heap_tree, HeapTree::Peak(_)) {
                " (peak)"
            } else {
                ""
            };
            let mut opened = true;
            egui::Window::new(format!("{}{suffix}", snap.id))
                .open(&mut opened)
                .show(ctx, |ui| {
                    egui::ScrollArea::vertical().show(ui, |ui| {
                        ui.code(snap.heap_tree.unwrap());
                    });
                });
            if !opened {
                remove_pos.push(pos);
            }
        }
        while let Some(pos) = remove_pos.pop() {
            self.opened_snapshots.remove(pos);
        }
    }
}

fn parse(s: &str) -> Vec<Snapshot> {
    // desc: (none)
    // cmd: target/debug/massif
    // time_unit: i
    // #-----------
    // snapshot=0
    // #-----------
    // time=0
    // mem_heap_B=0
    // mem_heap_extra_B=0
    // mem_stacks_B=0
    // heap_tree=empty

    let mut snapshots = Vec::new();
    let (_metadata, s) = s.split_once(DELIM).expect("a");
    let snaphot = s.split(DELIM).chunks(2);
    for mut data in &snaphot {
        let (id, snap) = data.next_tuple().expect("a");
        log::info!("id={id},snap={snap}");
        let (key, id) = id.split_once('=').expect("a");
        assert_eq!(key, "snapshot", "snapshot");
        log::info!("id={id}");
        let snap = Snapshot::from_str(snap, id.trim().parse().expect("msg"));
        snapshots.push(snap);
    }
    snapshots
}

#[derive(Default, serde::Deserialize, serde::Serialize)]
#[serde(default)]
struct Metadata {
    description: String,
    cmd: String,
    time_unit: String,
}

#[derive(serde::Deserialize, serde::Serialize)]
struct Snapshot {
    id: usize,
    time: usize,
    mem_heap_b: usize,
    mem_heap_extra_b: usize,
    mem_stacks_b: usize,
    heap_tree: HeapTree,
}

impl Snapshot {
    // snapshot=0
    // #-----------
    // time=0
    // mem_heap_B=0
    // mem_heap_extra_B=0
    // mem_stacks_B=0
    // heap_tree=empty
    fn from_str(s: &str, id: usize) -> Self {
        log::info!("{s:?}");
        let mut lines = s.lines();

        let (key, time) = lines.next().expect("a").split_once('=').expect("a");
        assert_eq!(key, "time", "time");
        let (key, mem_heap_b) = lines.next().expect("a").split_once('=').expect("a");
        assert_eq!(key, "mem_heap_B", "mem_heap_B");
        let (key, mem_heap_extra_b) = lines.next().expect("a").split_once('=').expect("a");
        assert_eq!(key, "mem_heap_extra_B", "mem_heap_extra_b");
        let (key, mem_stacks_b) = lines.next().expect("a").split_once('=').expect("a");
        assert_eq!(key, "mem_stacks_B", "mem_stacks_b");
        let (key, heap_tree) = lines.next().expect("a").split_once('=').expect("a");
        assert_eq!(key, "heap_tree", "heap_tree");
        let heap_tree = match heap_tree {
            "empty" => HeapTree::Empty,
            "peak" => HeapTree::Peak(lines.collect::<Vec<&str>>().join("")),
            "detailed" => HeapTree::Detailed(lines.collect::<Vec<&str>>().join("")),
            other => panic!("Unknown heap tree type {other}"),
        };

        Self {
            id,
            time: time.parse().expect("a"),
            mem_heap_b: mem_heap_b.parse().expect("a"),
            mem_heap_extra_b: mem_heap_extra_b.parse().expect("a"),
            mem_stacks_b: mem_stacks_b.parse().expect("a"),
            heap_tree,
        }
    }
}

#[derive(PartialEq, serde::Deserialize, serde::Serialize)]
enum HeapTree {
    Peak(String),
    Detailed(String),
    Empty,
}

impl HeapTree {
    fn unwrap(&self) -> &str {
        match self {
            HeapTree::Peak(s) | HeapTree::Detailed(s) => s,
            HeapTree::Empty => panic!("Called unwrap on an empty HeapTree"),
        }
    }
}
