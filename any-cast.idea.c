

struct any {
  void *data;
  void *type; // assuming we have Type *, just placeholder
} any;

int main(void) {
  
  any = (struct any) {
    .data = (int[]){0}, // We can use this trick to take pointers to literals.
    .type = nullptr,
  };

  // * the hard part of doing this is telling in the emitter when we actually
  // * need to emit this kind of implicit cast hack, since when an implicit cast is done,
  // * we just '(type)expr' cast it. and the original type gets lost.
  // * we might need some kind of implicit cast node or something.
}