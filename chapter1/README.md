clang++ -Xlinker --export-dynamic -g main.cpp -I../include $(llvm-config --cxxflags --ldflags --system-libs --libs core) -O3 -o main
