# CMake project for MUSIC V
cmake_minimum_required (VERSION 3.14 )
project(Music5 LANGUAGES C Fortran)

function(add_program NAME)
if(${ARGC} GREATER 1)
  add_executable(${NAME} "src/${NAME}.c")
else()
  add_executable(${NAME} "src/${NAME}.f")
endif()
   install(TARGETS ${NAME})
endfunction()

add_program(pass1)
add_program(pass2)
add_program(pass3)
add_program(raw2wav c)
add_program(music5 c)

enable_testing()

add_test(NAME ScoreCat513 COMMAND music5 ${CMAKE_CURRENT_SOURCE_DIR}/scores/cat513 cat513.wav)
