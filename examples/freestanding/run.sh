ela main.ela

nasm -felf64 ert.s -o ert.o
nasm -felf64 print.s -o print.o

ld print.o ert.o main.o -o main
rm print.o ert.o main.o

./main