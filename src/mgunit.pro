; docformat = 'rst'

;+
; Determines if the current terminal is a TTY, calling `MG_TERMISTTY` safely
; even if `cmdline_tools` is not installed or built.
;
; :Private:
;
; :Returns: 
;    1 if current term is a TTY, 0 if not
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
;    If there tests `test1_ut__define.pro` and test2_ut__define.pro` had been
;    created, then they could be run like::
;
;       IDL> mgunit, ['test1_ut', 'test2_ut']
;
;    Or one test could be run individually like::
;
;       IDL> mgunit, 'test1_ut'
; 
; :Params:
;    tests : in, optional, type=strarr
;       array of test suite and/or test case classnames
;
; :Keywords:
;    filename : in, optional, type=string
;       name of file to send output to; if not present sends output to the 
;       output log
;    color : in, optional, type=boolean
;       set to print color output to the output log
;    html : in, optional, type=boolean
;       set to indicate HTML output instead of plain text
;    xml : in, optional, type=boolean
;       set to indicate XML output instead of plain text
;    junit : in, optional, type=boolean
;       set to indicate XML output in JUnit format instead of plain text
;    gui : in, optional, type=boolean
;       set to bring up an interactive GUI to run the tests
;    npass : out, optional, type=long
;       number of tests that passed
;    nfail : out, optional, type=long
;       number of tests that failed
;    nskip : out, optional, type=long
;       number of tests that were skipped
;    ntests : out, optional, type=long
;       number of tests
;    failures_only : in, optional, type=boolean
;       report only failed tests
;    version : in, optional, type=boolean
;       set to report version and exit
;-
pro mgunit, tests, filename=filename, html=html, xml=xml, gui=gui, junit=junit, $
            color=color, $
            npass=npass, nfail=nfail, nskip=nskip, ntests=ntests, $
            failures_only=failuresOnly, version=version
  compile_opt strictarr

  if (keyword_set(version)) then begin
    print, string(mgunit_version(/full), format='(%"mgunit %s")')
    return
  endif

  case 1 of 
    keyword_set(gui): runnerName = 'MGutGuiRunner'
    keyword_set(html): runnerName = 'MGutHtmlRunner'
    keyword_set(xml): runnerName = 'MGutXmlRunner'
    keyword_set(junit): runnerName = 'MGutJUnitRunner'
    else: runnerName = 'MGutCliRunner'
  endcase
  
  _color = (keyword_set(color) || mgunit_findIfTty()) && (n_elements(filename) eq 0L)

  if (n_elements(tests) gt 0) then begin
    testRunner = obj_new('MGutCompoundRunner')
    
    npass = 0L
    nfail = 0L

    testsuite = obj_new('MGutTestSuite', $
                        test_runner=testRunner, $
                        name='All tests', $
                        failures_only=failuresOnly)

    testRunner->add, obj_new(runnerName, parent=testRunner, $
                             filename=filename, color=_color, $
                             test_suite=testsuite)
  
    testsuite->add, tests
    testsuite->run
    if (keyword_set(failuresOnly)) then testsuite->display
    testsuite->getProperty, npass=npass, nfail=nfail, nskip=nskip, $
                            ntests=ntests
    
    if (~keyword_set(gui)) then obj_destroy, testRunner
  endif
end
