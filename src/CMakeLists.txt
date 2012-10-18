execute_process(
  COMMAND sed "s/version = '.*'/version = '${VERSION}'/" mgunit_version.sed
  COMMAND sed "s/revision = '.*'/revision = '${REVISION}'/"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  OUTPUT_VARIABLE MGUNIT_VERSION_PRO
)
file(WRITE mgunit_version.pro "${MGUNIT_VERSION_PRO}")

install(
  FILES
    error_is_fail.pro
    error_is_pass.pro
    error_is_skip.pro
    style.css
    mgunit-templates.xml
  DESTINATION lib
)