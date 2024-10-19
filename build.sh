cargo run
llc-18 -filetype=obj output.bc -o output.o
objdump -M intel -d output.o
gcc output.o -o output -lc -no-pie