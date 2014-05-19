file(GLOB CONTRIBS ${PROJECT_SOURCE_DIR}/contrib/*/CMakeLists.txt)
foreach(contrib ${CONTRIBS})
  include(${contrib})
endforeach()

add_definitions("-DPIC_CONTRIB_INITS=${PICRIN_CONTRIB_INITS}")
