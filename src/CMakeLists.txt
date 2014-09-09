### libpicrin ###

find_package(Perl REQUIRED)

# benz
file(GLOB BENZ_SOURCES extlib/benz/*.c)

# srcs
file(GLOB PICRIN_SOURCES src/*.c)

# piclib
set(PICLIB_SOURCE ${PROJECT_SOURCE_DIR}/src/load_piclib.c)
add_custom_command(
  OUTPUT ${PICLIB_SOURCE}
  COMMAND ${PERL_EXECUTABLE} etc/mkloader.pl ${PICLIB_SCHEME_LIBS} ${PICLIB_CONTRIB_LIBS} > ${PICLIB_SOURCE}
  DEPENDS ${PICLIB_SCHEME_LIBS} ${PICLIB_CONTRIB_LIBS}
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
  )

# contrib
set(CONTRIB_INIT ${PROJECT_SOURCE_DIR}/src/init_contrib.c)
add_custom_command(
  OUTPUT ${CONTRIB_INIT}
  COMMAND ${PERL_EXECUTABLE} etc/mkinit.pl ${PICRIN_CONTRIB_INITS} > ${CONTRIB_INIT}
  DEPENDS ${PICRIN_CONTRIB_SOURCES}
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
  )

add_library(picrin SHARED ${BENZ_SOURCES} ${PICRIN_SOURCES} ${PICLIB_SOURCE} ${PICRIN_CONTRIB_SOURCES} ${CONTRIB_INIT})
target_link_libraries(picrin m ${PICRIN_CONTRIB_LIBRARIES})

# install
set(CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/lib)
install(TARGETS picrin DESTINATION lib)
install(DIRECTORY extlib/benz/include/ DESTINATION include FILES_MATCHING PATTERN "*.h")

### picrin ###

list(APPEND REPL_LIBRARIES picrin)

# build
add_executable(repl src/main.c)
set_target_properties(repl PROPERTIES OUTPUT_NAME picrin)
target_link_libraries(repl ${REPL_LIBRARIES})

# install
install(TARGETS repl RUNTIME DESTINATION bin)
