global print
extern strlen

; This is just a junky print written by chatgpt.

print:
  ; Save registers that will be used
  push rbx
  push rcx
  push rdx

  ; Load the format string
  mov rsi, rdi  ; format string

  ; Calculate the length of the string
  xor rdx, rdx
  call strlen
  mov rdx, rax

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