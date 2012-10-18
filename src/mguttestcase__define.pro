; docformat = 'rst'

;+
; Subclass `MGutTestCase` to actually write tests. In a subclass of 
; `MGutTestCase`, any function method whose name starts with "test" will be 
; considered a test. Tests are executed and results are reported to the test 
; runner object.
;
; :Examples:
;    To write your own tests, simply subclass from this class and make methods
;    that start with "test"::
;
;       pro mytest::test_myroutine
;         compile_opt strictarr
;          
;         answer = myroutine(1.0)   ; answer should be 2.
;         assert, abs(answer - 2.) lt 0.01, 'incorrect result, %f', answer
;
;         return, 1
;       end
;
;       pro mytest__define
;         compile_opt strictarr
;          
;         define = { mytest, inherits MGutTaseCase }
;       end
;
; :Properties:
;    npass : type=integer
;       number of passing tests
;    nfail : type=integer 
;       number of failing tests
;    nskip : type=integer 
;       number of skipped tests
;    ntests : type=integer
;       number of tests
;    testnames : type=strarr 
;       array of method names which begin with "test"
;-


;+
; Override in subclasses to perform setup actions before each test.
;-
pro mguttestcase::setup
  compile_opt strictarr

end


;+
; Override in subclasses to perform teardown actions after each test.
;-
pro mguttestcase::teardown
  compile_opt strictarr

end


;+
; Set this test to be skipped.
;
; :Private:
;-
pro mguttestcase::skip
  compile_opt strictarr
  
  self.skipped = 1
end


;+
; This is a safe place to actually run a single test. Any errors that occur 
; are assumed to be from the test and recorded as a failure for it.
;
; :Private:
;
; :Returns: 
;    boolean
;
; :Params:
;    testname : in, required, type=string
;       name of method
;
; :Keywords:
;    message : out, optional, type=string
;       error message if test failed
;-
function mguttestcase::runTest, testname, message=msg
  compile_opt strictarr, logical_predicate

  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    self.time = systime(/seconds) - self.time
    msg = !error_state.msg
    return, 0L   ; fail
  endif

  !error_state.msg = ''
  
  self.time = systime(/seconds)
  result = call_method(testname, self)
  self.time = systime(/seconds) - self.time
  
  if (~result) then msg = !error_state.msg
  return, keyword_set(result)
end


;+
; Run setup method before each test.
;
; :Private:
; 
; :Keywords:
;    fail : out, optional, type=boolean
;       set to a named variable to determine if the setup method failed
;-
pro mguttestcase::_runSetup, fail=fail
  compile_opt strictarr

  fail = 0L
  
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    fail = 1L
    return
  endif
    
  self->setup
end


;+
; Run teardown method before each test.
;
; :Private:
;
; :Keywords:
;    fail : out, optional, type=boolean
;       set to a named variable to determine if the teardown method failed
;-
pro mguttestcase::_runTeardown, fail=fail
  compile_opt strictarr

  fail = 0L
  
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    fail = 1L
    return
  endif
    
  self->teardown
end


;+
; Removes the given `prefix` from the `msg` if present.
; 
; :Private:
;
; :Params:
;    msg : in, required, type=string
;       string to remove prefix from, may be undefined
;    prefix : in, required, type=string
;       prefix to remove from msg
;-
pro mguttestcase::_removePrefix, msg, prefix
  compile_opt strictarr
  
  if (n_elements(msg) gt 0 && strpos(msg, prefix) eq 0) then begin
    prefixLength = strlen(prefix)
    msg = strmid(msg, prefixLength)
  endif
end


;+
; Display test results via test runner methods.
;
; :Private:
;-
pro mguttestcase::display
  compile_opt strictarr

  if (self.nfail eq 0L) then return
  
  self.testRunner->reportTestCaseStart, strlowcase(obj_class(self)), $
                                        ntests=self.ntests, $
                                        level=self.level
  for t = 0L, self.ntests - 1L do begin
    if ((*self.passes)[t] eq 0B) then begin
      self.testRunner->reportTestStart, (*self.testnames)[t], level=self.level
      self.testRunner->reportTestResult, (*self.logmsgs)[t], passed=(*self.passes)[t], $
                                         skipped=self.skipped, $
                                         time=self.time, level=self.level
    endif
  endfor

  self.testRunner->reportTestCaseResult, npass=self.npass, $
                                         nfail=self.nfail, $
                                         nskip=self.nskip, $
                                         level=self.level  
end


;+
; Run the tests for this class (i.e. methods with names that start with 
; "test").
;
; :Private:
;-
pro mguttestcase::run
  compile_opt strictarr, logical_predicate

  self.npass = 0L
  self.nfail = 0L
  self.nskip = 0L
  
  if (~self.failuresOnly) then begin
    self.testRunner->reportTestCaseStart, strlowcase(obj_class(self)), $
                                          ntests=self.ntests, $
                                          level=self.level
  endif

  ; run each test
  for t = 0L, self.ntests - 1L do begin
    if (~self.failuresOnly) then begin
      self.testRunner->reportTestStart, (*self.testnames)[t], level=self.level
    endif

    result = 0L         ; assume test failed
    setupFailed = 0L    ; assume setup/teardown worked unless otherwise told
    teardownFailed = 0L
    
    self->_runSetup, fail=setupFailed
    if (~setupFailed) then begin
      self.skipped = 0B
      result = self->runTest((*self.testnames)[t], message=msg)
      self->_runTeardown, fail=teardownFailed  
    endif
    
    passed = result && ~setupFailed && ~teardownFailed
    
    if (setupFailed) then begin
      msg = !error_state.msg
      self->_removePrefix, msg, 'ASSERT: '      
      msg = 'setup failed: ' + msg
    endif
    
    if (result && teardownFailed) then begin
      msg = !error_state.msg      
      self->_removePrefix, msg, 'ASSERT: '      
      msg = 'teardown failed: ' + msg    
    endif

    if (self.skipped) then begin
      msg = !error_state.msg
      self->_removePrefix, msg, 'ASSERT: '
      ++self.nskip
    endif else begin
      if (passed) then begin
        ++self.npass 
      endif else begin
        ++self.nfail
      endelse
    endelse
    
    ; remove method name from msg, if present
    self->_removePrefix, msg, obj_class(self) + '::' + (*self.testnames)[t] + ': '

    ; remove ASSERT from msg if present 
    self->_removePrefix, msg, 'ASSERT: '
    
    ; construct the log message for the test
    logMsg = (passed && ~self.skipped) $
             ? '' $ 
             : (n_elements(msg) eq 0 $
                ? '' $
                : msg)
                
    (*self.logmsgs)[t] = logMsg
    (*self.passes)[t] = passed
    if (~self.failuresOnly) then begin
      self.testRunner->reportTestResult, logMsg, passed=passed, $
                                         skipped=self.skipped, $
                                         time=self.time, level=self.level
    endif
  endfor

  if (~self.failuresOnly) then begin
    self.testRunner->reportTestCaseResult, npass=self.npass, $
                                           nfail=self.nfail, $
                                           nskip=self.nskip, $
                                           level=self.level
  endif
end


;+
; Find the name and number of tests (i.e. methods with names that start with 
; "test").
;
; :Private:
;-
pro mguttestcase::findTestnames
  compile_opt strictarr
  
  ; find tests: any method with name test*
  help, /routines, output=routines
  functionsPos = where(strmatch(routines, 'Compiled Functions:'), count)
  routines = routines[functionsPos:*]  
  result = stregex(routines, '^' + obj_class(self) + '::(test[^ ]*).*', $
                   /extract, /subexpr, /fold_case)
  testnames = reform(result[1, *])

  ; find names that matched
  ind = where(testnames ne '', ntests)
  if (ntests gt 0) then begin
    testnames = testnames[ind]
  endif

  ; record results
  self.ntests = ntests
  *self.testnames = strlowcase(testnames)
end


;+
; Get properties of the object.
;-
pro mguttestcase::getProperty, npass=npass, nfail=nfail, nskip=nskip, $
                               ntests=ntests, testnames=testnames
  compile_opt strictarr
  
  npass = self.npass
  nfail = self.nfail
  nskip = self.nskip
  ntests = self.ntests
  if (arg_present(testnames)) then testnames = *self.testnames
end


;+
; Set properties of the object.
;-
pro mguttestcase::setProperty, testnames=testnames
  compile_opt strictarr
  
  if (n_elements(testnames) gt 0L) then begin
    *self.testnames = strlowcase(testnames)
    self.ntests = n_elements(testnames)
  endif
end


;+
; Test suites can contain other test suites or test cases. The level is the
; number of layers down from the top most test suite (level 0).
;
; :Private:
;
; :Params:
;    level : in, required, type=integer
;       new level of object
;-
pro mguttestcase::setLevel, level
  compile_opt strictarr
  
  self.level = level
end


;+
; Free resources.
;-
pro mguttestcase::cleanup
  compile_opt strictarr

  ptr_free, self.testnames, self.logmsgs, self.passes
end


;+
; Intialize test case.
;
; :Returns:
;    1 for succcess, 0 for failure
; 
; :Keywords:
;    test_runner : in, required, type=object
;       subclass of `MGutTestRunner`
;    failures_only : in, optional, type=boolean
;       set to report only failed tests
;-
function mguttestcase::init, test_runner=testRunner, failures_only=failuresOnly
  compile_opt strictarr

  self.testRunner = testRunner
  self.failuresOnly = keyword_set(failuresOnly)
  
  self.testnames = ptr_new(/allocate_heap)
  self.logmsgs = ptr_new(/allocate_heap)
  self.passes = ptr_new(/allocate_heap)
  
  self->findTestnames

  *self.logmsgs = strarr(n_elements(*self.testnames))
  *self.passes = bytarr(n_elements(*self.testnames))
  
  self.level = 0L

  return, 1B
end


;+
; Define member variables.
;
; :Fields:
;    testRunner 
;       subclass of `MGtestRunner`
;    testnames 
;       pointer to string array of method names that start with "test"
;    logmsgs
;       pointer to string array of log message for each test
;    passes
;       point to byte array of pass/fail status for each test
;    level 
;       number of layers down from the top-containing suite
;    ntests 
;       total number of tests
;    npass 
;       number of passing tests
;    nfail 
;       number of failing tests
;    time
;       time for the current test to run
;    failuresOnly
;       flag to indicate only failed tests should be reported
;-
pro mguttestcase__define
  compile_opt strictarr

  define = { MGutTestCase, $
             testRunner: obj_new(), $
             testnames: ptr_new(), $
             logmsgs: ptr_new(), $
             passes: ptr_new(), $
             level: 0L, $
             ntests: 0L, $
             npass: 0L, $
             nfail: 0L, $
             nskip: 0L, $
             time: 0.0D, $
             skipped: 0B, $
             failuresOnly: 0B $             
             }             
end

