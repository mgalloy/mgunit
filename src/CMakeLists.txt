execute_process(
  COMMAND sed "s/version = '.*'/version = '${VERSION}'/" < mgunit_version.sed | sed "s/revision = '.*'/revision = '${REVISION}'/" > mgunit_version.pro
)
