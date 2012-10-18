execute_process(
  COMMAND sed "s/version = '.*'/version = '${VERSION}'/" mgunit_version.sed
  COMMAND sed "s/revision = '.*'/revision = '${REVISION}'/"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  OUTPUT_VARIABLE MGUNIT_VERSION_PRO
)
FILE(WRITE mgunit_version.pro "${MGUNIT_VERSION_PRO}")

