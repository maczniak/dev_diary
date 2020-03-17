# [Programming WebAssembly with Rust: Unified Development for Web, Mobile, and Embedded Applications][homepage] by Kevin Hoffman, Pragmatic Bookshelf (2019)

[errata][errata], [many Rust books (documents)][rust_doc], (Rust 1.31.0+)

I'm thoroughly convinced that the browser as a host for WebAssembly modules is
 just the tip of the iceberg.

[homepage]: https://pragprog.com/book/khrust/programming-webassembly-with-rust
[errata]: https://pragprog.com/titles/khrust/errata
[rust_doc]: https://www.rust-lang.org/learn

## Part I --- Building a Foundation

### 1. WebAssembly Fundamentals

[WebAssembly home page][webassembly]<br>
WebAssembly isn't run as a process outside the browser.<br>
[WebAssembly Studio][webassembly_studio]<br>
WebAssembly goes to great lengths to ensure that its control flow can't
 invalidate type safety, and can't be hijacked by attackers even with a
 ["heap corruption"][heap_corruption]-style attack in linear memory.<br>
WebAssembly doesn't have a heap in the traditional sense. There's no concept of
 a `new` operator. In fact, you don't allocate memory at the object level
 because there are no objects. There's also no garbage collection (at least not
 in the 1.0 MVP). Instead, WebAssembly has *linear memory*. This is a contiguous
 block of bytes that can be declared internally within the module, exported out
 of a module, or imported from the host. Your WebAssembly module can grow the
 limear memory block in increments called *pages* of 64KB. In addition to the
 efficiency of direct memory access, there's another reason why it's ideal for
 WebAssembly: *security*. While the host can read and write any linear memory
 given to a Wasm module at any time, the Wasm module can never access any of the
 host's memory.<br>
WebAssembly Binary Toolkit ([wabt GitHub][wabt]) - `wasm2c`, `wasm2wat`,
 `wasm-interp`, `wasm-objdump`, `wasm-opcodecnt`, `wasm-validate`, `wast2json`,
 `wat2wasm`, `wat-desugar`, `spectest-interp`, `wabt-unittests`<br>
WebAssembly doesn't have a string data type.

[webassembly]: https://webassembly.org/
[webassembly_studio]: https://webassembly.studio/
[heap_corruption]: https://pdfs.semanticscholar.org/14f1/4b032235c345dfb3b3ecc8a879bbe4072407.pdf
[wabt]: https://github.com/WebAssembly/wabt

### 2. Building WebAssembly Checkers

WebAssembly doesn't have arrays. It also doesn't have complex types.<br>
Some of the tools can be used to trim out some of the excess fat from these
 modules, but you'll never be able to create modules this small unless you're
 hand-coding your `wat`.

```webassembly
(if (result i32) (block (result i32) ...)
    (then ... )
    (else ... )
)
```

## Part II --- Interacting with JavaScript

### 3. Wading into WebAssembly with Rust

Rust is on a six-week release cycle for the stable toolchain.<br>
`rustup toolchain list`, `rustup target add wasm32-unknown-unknown`,
 `rustup target list`<br>
`cargo new --lib rustwasmhello`, `[lib] crate-type = ["cdylib"]`,
 `cargo build --release --target=wasm32-unknown-unknown`<br>
2015 edition, (Rust version 1.31.0 or newer)
 [`edition = "2018"`][2018_edition]<br>
Type `usize` is the data type Rust uses for vector indexes, allowing code to be
 more safe and portable across 32-bit and 64-bit architectures.<br>
Any assignment that isn't a reference is a *move*. Unless you explicitly treat
 something as a reference, ownership will be transferred during an assignment,
 meaning the value in the "old" variable will no longer be there.
 ([Rust's concept of ownership][rust_ownership])<br>
Calling `unwrap()` either grabs the value within `Ok(...)` or crashes the
 program upon failure. In production-grade applications, seek out and destroy
 all `unwrap`s with very few exceptions.<br>
The `into()` function shows one of my favorite aspects of Rust in action.
 Anything that implements the geneic trait called `Into<T>` for a given type can
 be converted *into* that type. While Rust doesn't have implicit conversions,
 its *explicit* conversions are quite powerful. The fact that the `self`
 variable here is not a reference means that the `GamePiece` will be *consumed*
 when converted into an integer.<br>
Rust code built for the `wasm32-unknown-unknown` target will default to putting
 function imports in the `env` namespace.

```webassembly
/*
[dependencies]
mut_static = "5.0.0"
lazy_static = "1.0.2"
*/

extern crate lazy_static;
use mut_static::MutStatic;

lazy_static! {
    pub static ref GAME_ENGINE: MutStatic<GameEngine> =
        { MutStatic::from(GameEngine::new()) };

// in a function
let engine = GAME_ENGINE.read().unwrap();

unsafe {
    notify_peicemoved(fx, fy, tx, ty);
}
```

[2018_edition]: https://doc.rust-lang.org/nightly/edition-guide/rust-2018/index.html
[rust_ownership]: https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html

### 4. Integrating WebAssembly with JavaScript

(Rust-specific) `wasm-bindgen` injects a bunch of metadata into your compiled
 WebAssembly module. Then, a separate command-line tool reads that metadata,
 strips it out, and uses that information to generate an appropriate JavaScript
 "wrapper bridge" containing the kinds of functions, classes, and other
 primitives that the developer wants bound to Rust.<br>
There are a couple of things that [Rot.js][rot_js] does quite well, like
 implementing pathfinding and random dungeon generation.
 [Rot.js tutorial][rot_js_tutorial]<br>
The [`js_sys`][js_sys] crate, which is part of wasm-bindgen, contains all of the
 mappings you need to global JavaScript functions. Instead of manually defining
 the binding for `alert()` like you did in this chapter, you can simply call
 `js_sys::alert()`. The `web_sys` rate has *all* of the
 [JavaScript Web API bindings][javascript_web_api] in it. Your rust code can
 make `web_sys` calls and, at runtime, the JavaScript host will make web API
 calls on your behalf.

```webassembly
// src/lib.rs
#[macro_use]
extern crate serde_derive;

extern crate wasm_bindgen;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    #[wasm_bindgen(module = "./index")]
    fn stats_updated(stats: JsValue);

    pub type Display;

    #[wasm_bindgen(method, structural, js_namespace = ROT)]
    fn draw(this: &Display, x: i32, y: i32, ch: &str);

    #[wasm_bindgen(method, structural, js_name = draw, js_namespace = ROT)]
    fn draw_color(this: &Display, x: i32, y: i32, ch: &str, color: &str);
}
```

```
$ npm run serve
> @ serve /home/kevin/Code/Rust/wasmbook/khrust/Book/code/jsint_roguewasm
> webpack-dev-server
```

[rot_js]: http://ondras.github.io/rot.js/manual/
[rot_js_tutorial]: http://www.roguebasin.com/index.php?title=Rot.js_tutorial
[js_sys]: https://rustwasm.github.io/wasm-bindgen/api/js_sys/
[javascript_web_api]: https://developer.mozilla.org/en-US/docs/Web/API

### 5. Advanced JavaScript Integration with Yew

[Yew][yew] 0.5.0 looks like it might contain some syntax improvements as well as
 multi-threaded worker support. Inspired by the *Elm* language and the *React*
 JavaScript framework, Yew is a modern web application framework designed to
 compile to WebAssembly, *asmjs*, or *emscripten*. Two main Yew traits are
 `Component` and `Renderable`. The Yew `Component` trait has `create()`,
 `update()`, `change()`, and `destroy()` functions. The Yew `Renderable` trait
 simply contains the `view()` function, which returns HTML that should be
 rendered to the client. State will be maintained entirely within your Rust
 code, and you won't have to worry about which part of it is a back-end
 component and which is front-end.<br>
`cargo install cargo-web`, `cargo web`<br>
Services, within the realm of Yew applications, are designed to expose
 "headless" functionality to UI components.

```webassembly
// src/main.rs
extern crate yew;
extern crate yewcounter;

use yew::prelude::*;
use yew::services::console::ConsoleService;
use yewcounter::Model;

pub struct Context {
    console: ConsoleService,
}

impl AsMut<ConsoleService> for Context {
    fn as_mut(&mut self) -> &mut ConsoleService {
        &mut self.console
    }
}

fn main() {
    yew::initialize();
    let context = Context {
        console: ConsoleService::new(),
    };
    let app: App<_, Model> = App:new(context);
    app.mount_to_body();
    yew::run_loop();
}

// src/lib.rs
impl<C> Component<C> for Model
    where
        C: AsMut<ConsoleService> {
        // ...
}

impl<C> Renderable<C, Model> for Model
    where
        C: AsMut<ConsoleSerivce> + 'static {
        // ...
}
```

[yew]: https://crates.io/crates/yew

## Part III --- Working with Non-Web Hosts

### 6. Hosting Modules Outside the Browser

### 7. Exploring the Internet of WebAssembly Things

### 8. Building WARoS---The WebAssembly Robot System

### A1. WebAssembly and Serverless

### A2. Securing WebAssembly Modules

