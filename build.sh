cargo run
llc-18 -filetype=obj output.bc -o output.o
objdump -M intel -d output.o
clang-18 output.o -o output -lc -no-pie