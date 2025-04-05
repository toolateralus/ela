## A list of features we want.

#### Compiler Features
-- Out of order compilation.
-- LLVM Backend.

#### Language Features
-- `Drop :: interface` destructors.
-- `choice` tagged unions.


Rework:
  get rid of ASTType and replace it with ASTPath that represents all :: expressions, all identifiers,
  all A::B!<T> etc