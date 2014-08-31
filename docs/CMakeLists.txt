# contribs
file(GLOB PICRIN_CONTRIB_DOCS ${PROJECT_SOURCE_DIR}/contrib/*/docs/*.rst)
file(GLOB PICRIN_DOCS ${PROJECT_SOURCE_DIR}/docs/*.rst)
list(SORT PICRIN_CONTRIB_DOCS)

set(PICRIN_CONTRIBS_DOC ${PROJECT_SOURCE_DIR}/docs/contrib.rst)
set(PICRIN_DOC_OUTPUT_DIRECTORY doc)

add_custom_command(
  OUTPUT ${PICRIN_CONTRIBS_DOC}
  COMMAND echo "Contrib Libraries \\\(a.k.a nitro\\\)" > ${PICRIN_CONTRIBS_DOC}
  COMMAND echo "===============================" >> ${PICRIN_CONTRIBS_DOC}
  COMMAND echo "" >> ${PICRIN_CONTRIBS_DOC}
  COMMAND cat ${PICRIN_CONTRIB_DOCS} >> ${PICRIN_CONTRIBS_DOC}
  DEPENDS ${PICRIN_CONTRIB_DOCS}
  )

add_custom_target(doc
  COMMAND make -C ${PROJECT_SOURCE_DIR}/docs html
  DEPENDS  ${PICRIN_CONTRIBS_DOC}
  )

add_custom_command(
  TARGET doc POST_BUILD
  COMMAND mkdir -p ${PICRIN_DOC_OUTPUT_DIRECTORY}
  COMMAND cp -uR  ${PROJECT_SOURCE_DIR}/docs/_build/* -t ${PICRIN_DOC_OUTPUT_DIRECTORY}/
  )