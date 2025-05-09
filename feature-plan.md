## A list of features we want.

#### Compiler Features
- Typed IR. this is very important and is a high priority; it will make our compiler much more stable and predictable I think,
  and simpler. Right now, the emitter(s) do a lot of resolution logic, and emit_symbol() is completely unreliable.

- Out of order compilation.

- LLVM Backend.

#### Language Features
-- `Drop :: trait` destructors. ((this is probably not possible due to our memory model. just gonna have to stick with deinit()!))
