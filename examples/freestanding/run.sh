ela main.ela --freestanding --release --s

if ! command -v nasm &> /dev/null
then
  echo -e "\e[31mWarning: nasm is not installed. Please install nasm to proceed, or rewrite the assembly for your target machine and use your own assembler\e[0m"
  exit 1
fi

nasm -felf64 ert.s -o ert.o
nasm -felf64 print.s -o print.o

ld print.o ert.o main.o -o main
rm print.o ert.o main.o

./main

tput sgr0
echo program exited: $?