section .text

global exit
global _start
extern main
; You could probably just do an `#export _start :: fn () {...}` but I'm not sure if the stack frame would be setup correctly.

_start:
  call main
  hlt

exit:
  mov rax, 60  ; syscall: exit
  mov rdi, rdi ; status: argument passed in rdi
  syscall
  