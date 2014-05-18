# flex
find_package(FLEX REQUIRED)
flex_target(scan src/scan.l ${PROJECT_SOURCE_DIR}/src/lex.yy.c COMPILE_FLAGS --header-file="src/lex.yy.h")
set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${PROJECT_SOURCE_DIR}/src/lex.yy.h)

# xfile
set(XFILE_SOURCES extlib/xfile/xfile.c)

# piclib
set(Python_ADDITIONAL_VERSIONS 2.7)
find_package(PYTHON REQUIRED)
set(PICLIB_SOURCES ${PROJECT_SOURCE_DIR}/src/load_piclib.c)
add_custom_command(
  OUTPUT ${PICLIB_SOURCES}
  COMMAND ${PYTHON_EXECUTABLE} etc/libemb.py
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
  )

# build!
file(GLOB PICRIN_SOURCES ${PROJECT_SOURCE_DIR}/src/*.c)
add_library(picrin SHARED ${PICRIN_SOURCES} ${PICLIB_SOURCES} ${FLEX_scan_OUTPUTS} ${XFILE_SOURCES} ${PICRIN_CONTRIB_SOURCES})
target_link_libraries(picrin m ${PICRIN_CONTRIB_LIBRARIES})

# install
install(TARGETS picrin DESTINATION lib)
install(DIRECTORY include/ DESTINATION include FILES_MATCHING PATTERN "*.h")
