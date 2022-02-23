# CMake project for MUSIC V
cmake_minimum_required (VERSION 2.8.12 )
project(Music5)
enable_language(Fortran)

# MUSIC 5 program
add_executable(music5 src/music5.c src/raw2wav.c src/pass1.f
src/pass2.f src/pass3.f)
install(TARGETS music5)
