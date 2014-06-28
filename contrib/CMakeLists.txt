file(GLOB CONTRIBS ${PROJECT_SOURCE_DIR}/contrib/*/CMakeLists.txt)
list(SORT CONTRIBS)
foreach(contrib ${CONTRIBS})
  include(${contrib})
endforeach()

add_definitions("-DPIC_CONTRIB_INITS=${PICRIN_CONTRIB_INITS}")
