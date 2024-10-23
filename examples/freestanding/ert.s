section .text


global exit

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
