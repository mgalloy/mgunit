catch, error
if (error ne 0L) then begin
  catch, /cancel
  assert, 0, mgunit_skip_msg, /skip
  return, 1
endif

on_ioerror, ioError
goto, startTest
ioError: assert, 0, mgunit_skip_msg, /skip
startTest:
