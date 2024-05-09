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

foreach(t IN ITEMS music5 raw2wav)
target_compile_definitions(${t} PRIVATE $<$<BOOL:${MSVC}>:_CRT_SECURE_NO_WARNINGS>)
endforeach()

enable_testing()

set(raw ${CMAKE_CURRENT_BINARY_DIR}/snd.raw)
set(out ${CMAKE_CURRENT_BINARY_DIR}/cat513.wav)

add_test(NAME ScoreCat513
COMMAND music5 ${CMAKE_CURRENT_SOURCE_DIR}/scores/cat513 ${out}
)
set_tests_properties(ScoreCat513 PROPERTIES
  FIXTURES_SETUP raw
)

add_test(NAME VerifyCat513
COMMAND ${CMAKE_COMMAND} -E sha256sum ${raw}
)
set_tests_properties(VerifyCat513 PROPERTIES
  FIXTURES_REQUIRED raw
  FIXTURES_SETUP out
  PASS_REGULAR_EXPRESSION "^8616d30732a65bebb821da9d2b8d710a55dd78d601279004817ef1680cf74e2d"
)

add_test(NAME VerifyWav513
COMMAND ${CMAKE_COMMAND} -E sha256sum ${out}
)
set_tests_properties(VerifyWav513 PROPERTIES
  FIXTURES_REQUIRED out
  PASS_REGULAR_EXPRESSION "^c02f8d8a6cd27166a64f59c5c045a5239a07aea26493c9e7a9aa5dcd8e019c11"
)

file(GENERATE OUTPUT .gitignore CONTENT "*")
