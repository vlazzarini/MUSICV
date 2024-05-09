# CMake project for MUSIC V
cmake_minimum_required (VERSION 3.17 )
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

set(pass1raw ${CMAKE_CURRENT_BINARY_DIR}/pass1.data)
set(pass2raw ${CMAKE_CURRENT_BINARY_DIR}/pass2.data)
set(score ${CMAKE_CURRENT_SOURCE_DIR}/scores/cat513)
set(raw ${CMAKE_CURRENT_BINARY_DIR}/snd.raw)
set(out ${CMAKE_CURRENT_BINARY_DIR}/cat513.wav)

add_test(NAME setupPass1 COMMAND ${CMAKE_COMMAND} -E copy ${score} ${CMAKE_CURRENT_BINARY_DIR}/score)
set_tests_properties(setupPass1 PROPERTIES FIXTURES_SETUP score)

add_test(NAME Pass1 COMMAND pass1 WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(Pass1 PROPERTIES
  FIXTURES_SETUP pass1
  FIXTURES_REQUIRED score
)

add_test(NAME VerifyPass1 COMMAND ${CMAKE_COMMAND} -E sha256sum ${pass1raw})
set_tests_properties(VerifyPass1 PROPERTIES
  FIXTURES_REQUIRED pass1
  FIXTURES_SETUP pass1raw
  REQUIRED_FILES ${pass1raw}
  PASS_REGULAR_EXPRESSION "^e14e58601899d39db63a3ce5e2152d4ead644ba44f143e319030688e82068dac "
)

add_test(NAME Pass2 COMMAND pass2 WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(Pass2 PROPERTIES
  FIXTURES_REQUIRED pass1raw
  FIXTURES_SETUP pass2
  REQUIRED_FILES ${pass1raw}
)

add_test(NAME VerifyPass2 COMMAND ${CMAKE_COMMAND} -E sha256sum ${CMAKE_CURRENT_BINARY_DIR}/pass2.data)
set_tests_properties(VerifyPass2 PROPERTIES
  FIXTURES_REQUIRED pass2
  FIXTURES_SETUP pass2raw
  REQUIRED_FILES ${pass2raw}
  PASS_REGULAR_EXPRESSION "^d440f0d7f0d34fdd7c4bf332a21aca57ce85d38024157cb932dacbe4c242104c"
)

add_test(NAME Pass3 COMMAND pass3 WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(Pass3 PROPERTIES
  FIXTURES_REQUIRED pass2raw
  FIXTURES_SETUP pass3
  REQUIRED_FILES ${pass2raw}
)

add_test(NAME VerifyPass3 COMMAND ${CMAKE_COMMAND} -E sha256sum ${raw})
set_tests_properties(VerifyPass3 PROPERTIES
  FIXTURES_REQUIRED pass3
  FIXTURES_SETUP pass3raw
  REQUIRED_FILES ${raw}
  PASS_REGULAR_EXPRESSION "^8616d30732a65bebb821da9d2b8d710a55dd78d601279004817ef1680cf74e2d"
)

add_test(NAME Pass3cleanup
COMMAND ${CMAKE_COMMAND} -E rm ${raw} ${CMAKE_CURRENT_BINARY_DIR}/score ${pass1raw} ${pass2raw})
set_tests_properties(Pass3cleanup PROPERTIES
  FIXTURES_CLEANUP pass3raw
  FIXTURES_SETUP clean
)

add_test(NAME ScoreCat513 COMMAND music5 ${score} ${out})
set_tests_properties(ScoreCat513 PROPERTIES
  FIXTURES_REQUIRED "clean;pass1raw;pass2raw"
  FIXTURES_SETUP raw
)

add_test(NAME VerifyCat513 COMMAND ${CMAKE_COMMAND} -E sha256sum ${raw})
set_tests_properties(VerifyCat513 PROPERTIES
  FIXTURES_REQUIRED raw
  FIXTURES_SETUP out
  REQUIRED_FILES ${raw}
  PASS_REGULAR_EXPRESSION "^8616d30732a65bebb821da9d2b8d710a55dd78d601279004817ef1680cf74e2d"
)

add_test(NAME VerifyWav513 COMMAND ${CMAKE_COMMAND} -E sha256sum ${out})
set_tests_properties(VerifyWav513 PROPERTIES
  FIXTURES_REQUIRED out
  REQUIRED_FILES ${out}
  PASS_REGULAR_EXPRESSION "^c02f8d8a6cd27166a64f59c5c045a5239a07aea26493c9e7a9aa5dcd8e019c11"
)

add_test(NAME MusicCleanup COMMAND ${CMAKE_COMMAND} -E rm ${out} ${raw})
set_tests_properties(MusicCleanup PROPERTIES
  FIXTURES_CLEANUP out
)

file(GENERATE OUTPUT .gitignore CONTENT "*")
