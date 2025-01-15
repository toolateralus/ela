section .text

global exit

; You could probably just do an `#export _start :: fn () {...}` but I'm not sure if the stack frame would be setup correctly.

exit:
  mov rax, 60  ; syscall: exit
  mov rdi, rdi ; status: argument passed in rdi
  syscall
  

global _start
extern main

_start:
  mov rbp, rsp
  call main
  mov rax, 60  ; syscall: exit
  xor rdi, rdi ; status: 0
  syscall
