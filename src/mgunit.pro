; docformat = 'rst'

;+
; Determines if the current terminal is a TTY, calling `MG_TERMISTTY` safely
; even if `cmdline_tools` is not installed or built.
;
; :Private:
;
; :Returns:
;   1 if current term is a TTY, 0 if not
;-
function mgunit_findIfTty
  compile_opt strictarr, hidden

  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    return, 0
  endif

  return, mg_termIsTty()
end


;+
; Runs unit tests provided.
;
; :Examples:
;   If test cases `my_routine1_ut__define.pro` and `my_routine2_ut__define.pro`
;   had been created, then they could be run like::
;
;     IDL> mgunit, ['my_routine1_ut', 'my_routine2_ut']
;
;   Or one test case could be run individually like::
;
;     IDL> mgunit, 'my_routine1_ut'
;
;   Even an individual test can be run, such as::
;
;     IDL> mgunit, 'my_routine1_ut.test_basic'
;
;
; :Params:
;   tests : in, optional, type=strarr
;     array of test suite and/or test case classnames, also may indicate
;     individual tests within a test case using a `.`
;
; :Keywords:
;   color : in, optional, type=boolean
;     set to print color output to the output log
;   filename : in, optional, type=string
;     name of file to send output to; if not present sends output to the
;     output log (can be an array if `RUNNERS` is an array)
;   html : in, optional, type=boolean
;     set to indicate HTML output instead of plain text
;   xml : in, optional, type=boolean
;     set to indicate XML output instead of plain text
;   junit : in, optional, type=boolean
;     set to indicate XML output in JUnit format instead of plain text
;   gui : in, optional, type=boolean
;     set to bring up an interactive GUI to run the tests
;   runners : in, optional, type=string/strarr
;     set to a string or string array specifying the test runners to use:
;     `MGutGuiRunner`, `MGutHtmlRunner`, `MGutXmlRunner`, `MGutJunitRunner`, or
;     `MGutCliRunner`
;   npass : out, optional, type=long
;     number of tests that passed
;   nfail : out, optional, type=long
;     number of tests that failed
;   nskip : out, optional, type=long
;     number of tests that were skipped
;   ntests : out, optional, type=long
;     number of tests
;   failures_only : in, optional, type=boolean
;     report only failed tests
;   version : in, optional, type=boolean
;     set to report version and exit
;   _extra : in, optional, type=keywords
;     keywords to `MGutTestCase` subclasses `init` methods
;-
pro mgunit, tests, $
            color=color, $
            filename=filename, $
            html=html, $
            xml=xml, $
            junit=junit, $
            gui=gui, $
            runners=runners, $
            npass=npass, $
            nfail=nfail, $
            nskip=nskip, $
            ntests=ntests, $
            failures_only=failuresOnly, $
            version=version, $
            _extra=e
  compile_opt strictarr

  if (keyword_set(version)) then begin
    print, string(mgunit_version(), format='(%"mgunit %s")')

    url = 'http://raw.github.com/mgalloy/mgunit/master/RELEASE.rst'
    needs_updating = mg_updater(url, $
                                current_version=mgunit_version(), $
                                name='mgunit', $
                                releases=releases, $
                                error=error, $
                                response_code=response_code, $
                                ssl_verify_peer=0, $
                                ssl_verify_host=0)
    if (error ne 0L) then begin
      print, 'Error checking for updates: ' + mg_responsecode_message(response_code)
    endif else begin
      if (needs_updating) then begin
        print, 'Updates available: ' + strjoin(releases[*].version, ', ')
      endif else begin
        print, 'No updates available'
      endelse
    endelse

    return
  endif

  case 1 of
    keyword_set(gui): runnerName = 'MGutGuiRunner'
    keyword_set(html): runnerName = 'MGutHtmlRunner'
    keyword_set(xml): runnerName = 'MGutXmlRunner'
    keyword_set(junit): runnerName = 'MGutJUnitRunner'
    else: runnerName = n_elements(runners) gt 0L ? runners : 'MGutCliRunner'
  endcase

  if (n_elements(tests) gt 0) then begin
    testRunner = obj_new('MGutCompoundRunner')

    npass = 0L
    nfail = 0L

    testsuite = obj_new('MGutTestSuite', $
                        test_runner=testRunner, $
                        name='All tests', $
                        failures_only=failuresOnly, $
                        _extra=e)

    for i = 0L, n_elements(runnerName) - 1L do begin
      f = n_elements(f) eq 0L ? 0L : i mod n_elements(filename)
      _filename = n_elements(filename) eq 0L ? '' : filename[f]
      _color = (keyword_set(color) || mgunit_findIfTty()) $
                 && (n_elements(filename) eq 0L || strlen(filename[f]) eq 0L)
      testRunner->add, obj_new(runnerName[i], $
                               parent=testRunner, $
                               filename=_filename, $
                               color=_color, $
                               test_suite=testsuite)
    endfor

    testsuite->add, tests, _extra=e
    testsuite->run
    if (keyword_set(failuresOnly)) then testsuite->display
    testsuite->getProperty, npass=npass, nfail=nfail, nskip=nskip, $
                            ntests=ntests

    if (~keyword_set(gui)) then obj_destroy, testRunner
  endif
end
