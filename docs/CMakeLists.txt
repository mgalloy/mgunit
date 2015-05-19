find_package(Docutils)
if (DOCUTILS_FOUND)
  add_custom_command(
    COMMAND ${RST2HTML_EXECUTABLE} --stylesheet=${CMAKE_CURRENT_SOURCE_DIR}/style.css ${CMAKE_CURRENT_SOURCE_DIR}/using-mgunit.rst ${CMAKE_CURRENT_BINARY_DIR}/using-mgunit.html
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/using-mgunit.rst
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/using-mgunit.html
  )
  add_custom_target(using ALL DEPENDS using-mgunit.html)

  install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/using-mgunit.html
    DESTINATION docs)
endif ()