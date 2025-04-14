### Argument/Return Type Semantics for LLVM Lowering

#### For Structs ≤ 8 Bytes:
**_if the type contains_...**
1. **All Integers/Pointers**:
   - Structs with a total size ≤ 4 bytes are passed as `i32`.
   - Structs with a total size > 4 bytes are passed as `i64`.

2. **All Floats**:
   - Structs with a total size ≤ 4 bytes are passed as `f32`.
   - Structs with a total size > 4 bytes are passed as `f64`.

3. **Mixed Floats and Integers**:
   - Structs with mixed float and integer fields are passed as `i64`.

#### For Structs > 8 Bytes:
- These are passed by reference, meaning a pointer to the struct is passed instead of the struct itself.

#### Return Values:
- The same rules apply as for argument passing:
  - Structs ≤ 8 bytes are returned as a single value (`i32`, `i64`, `f32`, or `f64`).
  - Structs > 8 bytes are returned by reference (via a pointer).

---

#### Special Case: Structs Between 8 and 16 Bytes
1. **Split into Two Chunks**:
   - Structs are divided into two chunks, each up to 8 bytes in size.

2. **Chunk Composition**:
   - If all fields are integers/pointers, the chunks are represented as two `i64` values.
   - If all fields are floats, the chunks are represented as two `f64` values.
   - If the fields are mixed, the chunks are represented as a combination (e.g., `i64` and `f64`).

3. **Reconstruction**:
   - To reconstruct the original struct, use GEPs to access the chunks and bit-shifting/masking to extract individual fields.

---

#### Example: Struct Reconstruction
For the struct:
```c
struct {
  u16 a;
  u16 b;
  u16 c;
  u16 d;
};
```

This struct would be split into two chunks:
```c
struct {
  i32 chunk1;
  i32 chunk2;
};
```

To reconstruct the fields:
1. Use GEPs to access `chunk1` and `chunk2`.
2. Extract individual fields using bit-shifting and masking:
   - `field1 = chunk1 & 0xFFFF`
   - `field2 = chunk1 >> 16`
   - `field3 = chunk2 & 0xFFFF`
   - `field4 = chunk2 >> 16`