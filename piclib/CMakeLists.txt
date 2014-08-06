list(APPEND PICLIB_SCHEME_LIBS
  ${PROJECT_SOURCE_DIR}/piclib/picrin/base.scm
  ${PROJECT_SOURCE_DIR}/piclib/picrin/list.scm
  ${PROJECT_SOURCE_DIR}/piclib/picrin/symbol.scm
  ${PROJECT_SOURCE_DIR}/piclib/picrin/macro.scm

  ${PROJECT_SOURCE_DIR}/piclib/scheme/base.scm

  ${PROJECT_SOURCE_DIR}/piclib/picrin/array.scm
  ${PROJECT_SOURCE_DIR}/piclib/picrin/dictionary.scm
  ${PROJECT_SOURCE_DIR}/piclib/picrin/test.scm
  ${PROJECT_SOURCE_DIR}/piclib/picrin/experimental/lambda.scm

  ${PROJECT_SOURCE_DIR}/piclib/scheme/cxr.scm
  ${PROJECT_SOURCE_DIR}/piclib/scheme/file.scm
  ${PROJECT_SOURCE_DIR}/piclib/scheme/case-lambda.scm
  ${PROJECT_SOURCE_DIR}/piclib/scheme/lazy.scm
  ${PROJECT_SOURCE_DIR}/piclib/scheme/eval.scm
  ${PROJECT_SOURCE_DIR}/piclib/scheme/r5rs.scm
  ${PROJECT_SOURCE_DIR}/piclib/scheme/null.scm

  ${PROJECT_SOURCE_DIR}/piclib/srfi/1.scm
  ${PROJECT_SOURCE_DIR}/piclib/srfi/8.scm
  ${PROJECT_SOURCE_DIR}/piclib/srfi/26.scm
  ${PROJECT_SOURCE_DIR}/piclib/srfi/43.scm
  ${PROJECT_SOURCE_DIR}/piclib/srfi/60.scm
  ${PROJECT_SOURCE_DIR}/piclib/srfi/95.scm
  ${PROJECT_SOURCE_DIR}/piclib/srfi/111.scm

  ${PROJECT_SOURCE_DIR}/piclib/picrin/user.scm
  ${PROJECT_SOURCE_DIR}/piclib/picrin/repl.scm
  )
