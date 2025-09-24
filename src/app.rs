use egui::{
    Color32, CornerRadius, FontFamily, PointerButton, PopupAnchor, Pos2, Rect, RichText, Scene,
    Sense, Stroke, StrokeKind, Tooltip, Ui, Vec2,
    emath::RectTransform,
    text::{LayoutJob, TextWrapping},
};
use egui_plot::{Line, MarkerShape, Plot, PlotPoints, Points};
use itertools::Itertools;
use regex::Regex;

const DELIM: &str = "#-----------\n";

#[derive(Default, serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct App {
    input: String,
    snapshots: Vec<Snapshot>,
    #[serde(default)]
    opened_snapshots: Vec<Window>,
    plot: bool,
}

#[derive(serde::Deserialize, serde::Serialize)]
struct Window {
    index: usize,
    scene_rect: Rect,
    filter_string: String,
    #[serde(skip, default)]
    filter_regex: Option<Result<Regex, regex::Error>>,
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
                Plot::new("my_plot")
                    .x_axis_label("i")
                    .y_axis_formatter(|mark, _range| {
                        if mark.value.is_sign_positive() {
                            bity::byte::format(mark.value as u64)
                        } else {
                            format!("-{}", bity::byte::format((-mark.value) as u64))
                        }
                    })
                    .view_aspect(2.0)
                    .show(ui, |plot_ui| {
                        if plot_ui.response().clicked_by(PointerButton::Primary)
                            && let Some(pos) = plot_ui.pointer_coordinate()
                        {
                            let (pos, _snap) = self
                                .snapshots
                                .iter()
                                .enumerate()
                                .filter(|snap| !matches!(snap.1.heap_tree, HeapTree::Empty))
                                .min_by_key(|snap| snap.1.id.abs_diff(pos.x as usize))
                                .unwrap();
                            if self.snapshots[pos].heap_tree != HeapTree::Empty {
                                self.opened_snapshots.push(Window {
                                    index: pos,
                                    scene_rect: Rect::ZERO,
                                    filter_string: String::new(),
                                    filter_regex: None,
                                });
                            }
                        }
                        plot_ui.line(line);
                        plot_ui.points(peaks);
                        plot_ui.points(detailed);
                    });
            }
        });
        let mut remove_pos = Vec::new();
        for (
            pos,
            Window {
                index,
                scene_rect,
                filter_string,
                filter_regex,
            },
        ) in self.opened_snapshots.iter_mut().enumerate()
        {
            let snap = &self.snapshots[*index];
            let suffix = if matches!(snap.heap_tree, HeapTree::Peak(_)) {
                " (peak)"
            } else {
                ""
            };
            let mut opened = true;
            egui::Window::new(format!("{}{suffix}", snap.id))
                .open(&mut opened)
                .show(ctx, |ui| {
                    let response = egui::MenuBar::new().ui(ui, |ui| {
                        ui.label("Filter (regex):");
                        let response = ui.text_edit_singleline(filter_string);
                        if response.changed() {
                            if filter_string.is_empty() {
                                *filter_regex = None;
                            } else {
                                *filter_regex = Some(Regex::new(filter_string));
                            }
                        }

                        if let Some(Err(e)) = filter_regex {
                            Tooltip::always_open(
                                ui.ctx().clone(),
                                response.layer_id,
                                response.id,
                                PopupAnchor::ParentRect(response.rect),
                            )
                            .show(|ui| {
                                ui.label(
                                    RichText::new(e.to_string()).monospace().color(Color32::RED),
                                );
                            });
                        }
                    });
                    let max_rect = ui.max_rect();
                    let max_size = max_rect.max - max_rect.min;
                    let scene = Scene::new().max_inner_size(max_size).zoom_range(0.1..=20.0);
                    let transform = RectTransform::from_to(
                        Rect {
                            min: Pos2 {
                                x: ui.max_rect().min.x,
                                y: response.response.rect.max.y,
                            },
                            max: ui.max_rect().max,
                        },
                        *scene_rect,
                    );
                    let mouse_pos = ui
                        .ctx()
                        .pointer_hover_pos()
                        .filter(|pos| ui.max_rect().contains(*pos))
                        .map(|pos| transform.transform_pos(pos));

                    let heap_node = HeapNode::try_from(snap.heap_tree.unwrap()).unwrap();
                    scene.show(ui, scene_rect, |ui| {
                        heap_node.paint(
                            ui,
                            mouse_pos,
                            filter_regex.as_ref().map(|n| n.as_ref().ok()).flatten(),
                        )
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
            "peak" => HeapTree::Peak(lines.collect::<Vec<&str>>().join("\n")),
            "detailed" => HeapTree::Detailed(lines.collect::<Vec<&str>>().join("\n")),
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

#[derive(Clone, Debug)]
struct HeapNode<'a> {
    bytes: usize,
    addr: Option<usize>,
    name: &'a str,
    location: Option<&'a str>,
    children: Vec<HeapNode<'a>>,
}

struct DisplayStep<'a> {
    base: Pos2,
    width: f32,
    node: &'a HeapNode<'a>,
}

impl<'a> HeapNode<'a> {
    fn parse(lines: &mut impl Iterator<Item = &'a str>) -> Option<Self> {
        let line = lines.next().expect("missing heap node line");

        // Parse children count.
        let (children_count, remaining) = line.split_once(": ").unwrap();
        let children_count = children_count
            .strip_prefix('n')
            .unwrap()
            .parse::<usize>()
            .unwrap();

        // Special case for bellow threshold nodes.
        if children_count == 0 && remaining.contains("all below massif's threshold") {
            return None;
        }

        // Parse bytes.
        let (bytes, remaining) = remaining.split_once(' ').unwrap();
        let bytes = bytes.parse().unwrap();

        // Parse memory address.
        let (addr, name, location) = if remaining.starts_with("0x") {
            let (addr, remaining) = remaining.split_once(": ").unwrap();
            let (name, location) = remaining.rsplit_once(" (").unwrap();
            (
                Some(usize::from_str_radix(addr.trim_start_matches("0x"), 16).unwrap()),
                name,
                Some(location.trim_end_matches(')')),
            )
        } else {
            (None, remaining, None)
        };

        // Parse children.
        let children = (0..children_count)
            .filter_map(|_| Self::parse(lines))
            .collect();

        Some(Self {
            bytes,
            addr,
            name,
            location,
            children,
        })
    }

    fn paint(&self, ui: &mut Ui, mouse_pos: Option<Pos2>, filter: Option<&Regex>) {
        const HEIGHT: f32 = 16.0;

        let (response, painter) =
            ui.allocate_painter(ui.available_size_before_wrap(), Sense::drag());
        let mut rect = response.rect;
        rect.min += Vec2::splat(1.0);
        rect.max -= Vec2::splat(1.0);
        let font = egui::FontId {
            size: HEIGHT - 1.0,
            family: FontFamily::Monospace,
        };
        let visual = ui.visuals();
        let color = visual.text_color();
        let filter_match_bg_color = if visual.dark_mode {
            Color32::DARK_GREEN
        } else {
            Color32::LIGHT_GREEN
        };
        let galley = painter.layout_no_wrap("...".to_string(), font.clone(), color);
        let min_text_size_to_display = galley.size().x;

        let mut explore = vec![DisplayStep {
            base: rect.left_bottom(),
            width: rect.width(),
            node: self,
        }];

        while let Some(DisplayStep { base, width, node }) = explore.pop() {
            let display = format!("{} ({})", node.name, node.location.unwrap_or_default());
            let background_color = if let Some(regex) = filter
                && regex.is_match(&display)
            {
                filter_match_bg_color
            } else {
                Color32::TRANSPARENT
            };
            let rect = Rect {
                min: Pos2 {
                    x: base.x,
                    y: base.y - HEIGHT,
                },
                max: Pos2 {
                    x: base.x + width,
                    y: base.y,
                },
            };
            painter.rect(
                rect,
                CornerRadius::same(2),
                background_color,
                Stroke::new(1.0, color),
                StrokeKind::Middle,
            );
            if let Some(mouse_pos) = mouse_pos
                && rect.contains(mouse_pos)
            {
                Tooltip::always_open(
                    ui.ctx().clone(),
                    response.layer_id,
                    response.id,
                    PopupAnchor::Pointer,
                )
                .show(|ui| {
                    ui.horizontal(|ui| {
                        ui.label(RichText::new(node.name).strong());
                        if let Some(addr) = node.addr {
                            ui.label(format!(" 0x{:08x}", addr));
                        }
                    });
                    if let Some(location) = node.location {
                        ui.label(format!(" ({})", location));
                    }
                });
            }

            if width > min_text_size_to_display {
                let wrapping = TextWrapping::truncate_at_width(width - HEIGHT);
                let mut layout = LayoutJob::simple_singleline(display, font.clone(), color);
                layout.wrap = wrapping;
                let galley = painter.layout_job(layout);
                painter.galley(
                    Pos2 {
                        x: base.x + HEIGHT / 2.0,
                        y: base.y - HEIGHT,
                    },
                    galley,
                    color,
                );
            }
            let mut current_x = base.x;
            for child in node.children.iter() {
                let child_width = ((child.bytes as f32) / (node.bytes as f32)) * width;
                explore.push(DisplayStep {
                    base: Pos2 {
                        x: current_x,
                        y: base.y - HEIGHT,
                    },
                    width: child_width,
                    node: child,
                });
                current_x += child_width;
            }
        }
    }

    /// Calculate recursive number of children nodes.
    #[cfg(test)]
    fn children_count(&self) -> usize {
        self.children.len()
            + self
                .children
                .iter()
                .map(Self::children_count)
                .sum::<usize>()
    }

    #[cfg(test)]
    fn children_recursive(&self) -> Vec<&[Self]> {
        let mut nodes_children = vec![&*self.children];
        nodes_children.extend(self.children.iter().flat_map(Self::children_recursive));
        nodes_children
    }
}

impl<'a> TryFrom<&'a str> for HeapNode<'a> {
    type Error = ();

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Self::parse(&mut value.lines().map(str::trim)).ok_or(())
    }
}

#[cfg(test)]
mod tests {
    use crate::app::HeapNode;

    #[test]
    fn heap_node() {
        let input = r###"
n3: 1792 (heap allocation functions) malloc/new/new[], --alloc-fns, etc.
 n1: 1024 0x12FE91: alloc (alloc.rs:93)
  n1: 1024 0x12FE91: alloc_impl (alloc.rs:188)
   n1: 1024 0x12FE91: allocate (alloc.rs:249)
    n1: 1024 0x12FE91: try_allocate_in<alloc::alloc::Global> (mod.rs:476)
     n1: 1024 0x12FE91: with_capacity_in<alloc::alloc::Global> (mod.rs:422)
      n1: 1024 0x12FE91: with_capacity_in<u8, alloc::alloc::Global> (mod.rs:190)
       n1: 1024 0x12FE91: with_capacity_in<u8, alloc::alloc::Global> (mod.rs:815)
        n1: 1024 0x12FE91: with_capacity<u8> (mod.rs:495)
         n1: 1024 0x12FE91: with_capacity<std::io::stdio::StdoutRaw> (bufwriter.rs:122)
          n1: 1024 0x12FE91: with_capacity<std::io::stdio::StdoutRaw> (linewriter.rs:110)
           n1: 1024 0x12FE91: new<std::io::stdio::StdoutRaw> (linewriter.rs:90)
            n1: 1024 0x12FE91: {closure#0} (stdio.rs:719)
             n1: 1024 0x12FE91: {closure#0}<std::sync::reentrant_lock::ReentrantLock<core::cell::RefCell<std::io::buffered::linewriter::LineWriter<std::io::stdio::StdoutRaw>>>, std::io::stdio::stdout::{closure_env#0}> (once_lock.rs:310)
              n1: 1024 0x12FE91: {closure#0}<std::sync::reentrant_lock::ReentrantLock<core::cell::RefCell<std::io::buffered::linewriter::LineWriter<std::io::stdio::StdoutRaw>>>, std::sync::once_lock::{impl#0}::get_or_init::{closure_env#0}<std::sync::reentrant_lock::ReentrantLock<core::cell::RefCell<std::io::buffered::linewriter::LineWriter<std::io::stdio::StdoutRaw>>>, std::io::stdio::stdout::{closure_env#0}>, !> (once_lock.rs:518)
               n1: 1024 0x12FE91: std::sync::poison::once::Once::call_once_force::{{closure}} (once.rs:214)
                n1: 1024 0x10EAF3: std::sys::sync::once::futex::Once::call (futex.rs:176)
                 n1: 1024 0x10E571: call_once_force<std::sync::once_lock::{impl#0}::initialize::{closure_env#0}<std::sync::reentrant_lock::ReentrantLock<core::cell::RefCell<std::io::buffered::linewriter::LineWriter<std::io::stdio::StdoutRaw>>>, std::sync::once_lock::{impl#0}::get_or_init::{closure_env#0}<std::sync::reentrant_lock::ReentrantLock<core::cell::RefCell<std::io::buffered::linewriter::LineWriter<std::io::stdio::StdoutRaw>>>, std::io::stdio::stdout::{closure_env#0}>, !>> (once.rs:214)
                  n1: 1024 0x10E571: std::sync::once_lock::OnceLock<T>::initialize (once_lock.rs:517)
                   n1: 1024 0x12D633: get_or_try_init<std::sync::reentrant_lock::ReentrantLock<core::cell::RefCell<std::io::buffered::linewriter::LineWriter<std::io::stdio::StdoutRaw>>>, std::sync::once_lock::{impl#0}::get_or_init::{closure_env#0}<std::sync::reentrant_lock::ReentrantLock<core::cell::RefCell<std::io::buffered::linewriter::LineWriter<std::io::stdio::StdoutRaw>>>, std::io::stdio::stdout::{closure_env#0}>, !> (once_lock.rs:396)
                    n1: 1024 0x12D633: get_or_init<std::sync::reentrant_lock::ReentrantLock<core::cell::RefCell<std::io::buffered::linewriter::LineWriter<std::io::stdio::StdoutRaw>>>, std::io::stdio::stdout::{closure_env#0}> (once_lock.rs:310)
                     n1: 1024 0x12D633: stdout (stdio.rs:719)
                      n1: 1024 0x12D633: print_to<std::io::stdio::Stdout> (stdio.rs:1164)
                       n1: 1024 0x12D633: std::io::stdio::_print (stdio.rs:1275)
                        n1: 1024 0x10FF7C: massif::main (main.rs:7)
                         n1: 1024 0x110A8A: core::ops::function::FnOnce::call_once (function.rs:250)
                          n1: 1024 0x1105CD: std::sys::backtrace::__rust_begin_short_backtrace (backtrace.rs:152)
                           n1: 1024 0x110080: std::rt::lang_start::{{closure}} (rt.rs:199)
                            n1: 1024 0x12B6B3: call_once<(), (dyn core::ops::function::Fn<(), Output=i32> + core::marker::Sync + core::panic::unwind_safe::RefUnwindSafe)> (function.rs:284)
                             n1: 1024 0x12B6B3: do_call<&(dyn core::ops::function::Fn<(), Output=i32> + core::marker::Sync + core::panic::unwind_safe::RefUnwindSafe), i32> (panicking.rs:589)
                              n1: 1024 0x12B6B3: try<i32, &(dyn core::ops::function::Fn<(), Output=i32> + core::marker::Sync + core::panic::unwind_safe::RefUnwindSafe)> (panicking.rs:552)
                               n1: 1024 0x12B6B3: catch_unwind<&(dyn core::ops::function::Fn<(), Output=i32> + core::marker::Sync + core::panic::unwind_safe::RefUnwindSafe), i32> (panic.rs:359)
                                n1: 1024 0x12B6B3: {closure#0} (rt.rs:168)
                                 n1: 1024 0x12B6B3: do_call<std::rt::lang_start_internal::{closure_env#0}, isize> (panicking.rs:589)
                                  n1: 1024 0x12B6B3: try<isize, std::rt::lang_start_internal::{closure_env#0}> (panicking.rs:552)
                                   n1: 1024 0x12B6B3: catch_unwind<std::rt::lang_start_internal::{closure_env#0}, isize> (panic.rs:359)
                                    n1: 1024 0x12B6B3: std::rt::lang_start_internal (rt.rs:164)
                                     n1: 1024 0x110066: std::rt::lang_start (rt.rs:198)
                                      n0: 1024 0x10FFDD: main (in /home/ubuntu/benjamin/massif/target/debug/massif)
 n1: 768 0x10E1C1: realloc (alloc.rs:133)
  n1: 768 0x10E1C1: grow_impl (alloc.rs:221)
   n1: 768 0x10E1C1: grow (alloc.rs:283)
    n1: 768 0x10E1C1: alloc::raw_vec::finish_grow (mod.rs:781)
     n1: 768 0x11BB60: grow_amortized<alloc::alloc::Global> (mod.rs:664)
      n1: 768 0x11BB60: grow_one<alloc::alloc::Global> (mod.rs:571)
       n1: 768 0x11BB60: alloc::raw_vec::RawVec<T,A>::grow_one (mod.rs:340)
        n1: 768 0x11033E: alloc::vec::Vec<T,A>::push (mod.rs:2448)
         n1: 768 0x10FF59: massif::main (main.rs:6)
          n1: 768 0x110A8A: core::ops::function::FnOnce::call_once (function.rs:250)
           n1: 768 0x1105CD: std::sys::backtrace::__rust_begin_short_backtrace (backtrace.rs:152)
            n1: 768 0x110080: std::rt::lang_start::{{closure}} (rt.rs:199)
             n1: 768 0x12B6B3: call_once<(), (dyn core::ops::function::Fn<(), Output=i32> + core::marker::Sync + core::panic::unwind_safe::RefUnwindSafe)> (function.rs:284)
              n1: 768 0x12B6B3: do_call<&(dyn core::ops::function::Fn<(), Output=i32> + core::marker::Sync + core::panic::unwind_safe::RefUnwindSafe), i32> (panicking.rs:589)
               n1: 768 0x12B6B3: try<i32, &(dyn core::ops::function::Fn<(), Output=i32> + core::marker::Sync + core::panic::unwind_safe::RefUnwindSafe)> (panicking.rs:552)
                n1: 768 0x12B6B3: catch_unwind<&(dyn core::ops::function::Fn<(), Output=i32> + core::marker::Sync + core::panic::unwind_safe::RefUnwindSafe), i32> (panic.rs:359)
                 n1: 768 0x12B6B3: {closure#0} (rt.rs:168)
                  n1: 768 0x12B6B3: do_call<std::rt::lang_start_internal::{closure_env#0}, isize> (panicking.rs:589)
                   n1: 768 0x12B6B3: try<isize, std::rt::lang_start_internal::{closure_env#0}> (panicking.rs:552)
                    n1: 768 0x12B6B3: catch_unwind<std::rt::lang_start_internal::{closure_env#0}, isize> (panic.rs:359)
                     n1: 768 0x12B6B3: std::rt::lang_start_internal (rt.rs:164)
                      n1: 768 0x110066: std::rt::lang_start (rt.rs:198)
                       n0: 768 0x10FFDD: main (in /home/ubuntu/benjamin/massif/target/debug/massif)
 n0: 0 in 9 places, all below massif's threshold (1.00%)
        "###.trim();

        let root = HeapNode::try_from(input).unwrap();
        assert_eq!(root.children.len(), 2);
        assert_eq!(1 + root.children_count(), 62);
        // Verify that there is at most one child per node, except of the root node which has two.
        assert_eq!(root.children.len(), 2);
        assert!(root.children.iter().all(|direct_children| {
            direct_children
                .children_recursive()
                .into_iter()
                .all(|children| children.len() <= 1)
        }));
    }
}
