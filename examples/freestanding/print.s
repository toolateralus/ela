global print

print:
  ; Save registers that will be used
  push rbx
  push rcx
  push rdx

  ; Load the format string
  mov rsi, rdi  ; format string

  ; Calculate the length of the string
  xor rdx, rdx
find_len:
  cmp byte [rsi + rdx], 0
  je print_string
  inc rdx
  jmp find_len

print_string:
  ; Print the format string
  mov rax, 1           ; syscall: write
  mov rdi, 1           ; file descriptor: stdout
  syscall

  ; Restore registers
  pop rdx
  pop rcx
  pop rbx
  ret