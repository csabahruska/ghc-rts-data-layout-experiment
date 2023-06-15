set -x -e

#gcc a.c -E > a.i

# preferred
gcc     a.c -save-temps=obj -o out/a-output-obj-gcc.o -S
clang-9 a.c -save-temps=obj -o out/a-output-obj-clang.o -S

globvars out/a-output-obj-gcc.i
