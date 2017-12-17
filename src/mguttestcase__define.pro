; docformat = 'rst'

;+
; Subclass `MGutTestCase` to actually write tests. In a subclass of
; `MGutTestCase`, any function method whose name starts with "test" will be
; considered a test. Tests are executed and results are reported to the test
; runner object.
;
; :Examples:
;   To write your own tests, simply subclass from this class and make methods
;   that start with "test"::
;
;     pro mytest::test_myroutine
;       compile_opt strictarr
;
;       answer = myroutine(1.0)   ; answer should be 2.
;       assert, abs(answer - 2.) lt 0.01, 'incorrect result, %f', answer
;
;       return, 1
;     end
;
;     pro mytest__define
;       compile_opt strictarr
;
;       define = { mytest, inherits MGutTaseCase }
;     end
;
; :Properties:
;   npass : type=integer
;     number of passing tests
;   nfail : type=integer
;     number of failing tests
;   nskip : type=integer
;     number of skipped tests
;   ntests : type=integer
;     number of tests
;   testnames : type=strarr
;     array of method names which begin with "test"
;   have_output : bytarr
;     array of whether tests return output
;   math_errors : type=integer
;     bitmask of `CHECK_MATH` return values
;   testing_routines : type=strarr
;     routine names of tested routines
;   total_nlines : type=long
;     total number of lines in testing routines
;   covered_nlines : type=long
;     number of tested lines in testing routines
;-


;= abstract fixture methods

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


;= running the test

;+
; This is a safe place to actually run a single test. Any errors that occur
; are assumed to be from the test and recorded as a failure for it.
;
; :Private:
;
; :Returns:
;   boolean
;
; :Params:
;   testname : in, required, type=string
;     name of method
;
; :Keywords:
;   message : out, optional, type=string
;     error message if test failed
;   has_output : in, optional, type=boolean
;     set to indicate the called routine has an `OUTPUT` keyword
;   output : out, optional, type=string
;     output from the called routine, if any
;-
function mguttestcase::runTest, testname, message=msg, $
                                has_output=has_output, output=output
  compile_opt strictarr, logical_predicate

  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    self.time = systime(/seconds) - self.time
    msg = !error_state.msg
    return, 0L   ; fail
  endif

  !error_state.msg = ''

  old_except = !except
  !except = 0
  math_errors = check_math()

  self.time = systime(/seconds)
  if (has_output) then begin
    result = call_method(testname, self, output=output)
  endif else begin
    result = call_method(testname, self)
  endelse
  self.time = systime(/seconds) - self.time

  self.math_errors = check_math()
  !except = old_except

  if (~result) then msg = !error_state.msg
  return, keyword_set(result)
end


;+
; Run setup method before each test.
;
; :Private:
;
; :Keywords:
;   fail : out, optional, type=boolean
;     set to a named variable to determine if the setup method failed
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
;   fail : out, optional, type=boolean
;     set to a named variable to determine if the teardown method failed
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
;   msg : in, required, type=string
;     string to remove prefix from, may be undefined
;   prefix : in, required, type=string
;     prefix to remove from msg
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
    if ((*self.passes)[t] eq 0B && (*self.skips)[t] eq 0B) then begin
      self.testRunner->reportTestStart, (*self.testnames)[t], level=self.level
      self.testRunner->reportTestResult, (*self.logmsgs)[t], $
                                         passed=(*self.passes)[t], $
                                         output=(*self.output)[t], $
                                         skipped=(*self.skips)[t], $
                                         time=self.time, level=self.level, $
                                         math_errors=self.math_errors
    endif
  endfor

  self.testRunner->reportTestCaseResult, npass=self.npass, $
                                         nfail=self.nfail, $
                                         nskip=self.nskip, $
                                         level=self.level  
end


;+
; Create a string from an array of line numbers.
;
; :Private:
;
; :Returns:
;   string
;
; :Params:
;   lines : in, required, type=long/`lonarr`
;     array of line numbers or a scalar 0
;-
function mguttestcase::_combineLines, lines
  compile_opt strictarr

  if (size(lines, /n_dimensions) eq 0) then return, ''

  f = '(%"%d")'
  if (n_elements(lines) eq 1L) then return, string(lines[0], format=f)

  diff = lines[1:-1] - lines[0:-2]
  r = string(lines[0], format=f)
  in_range = 0B
  for i = 0L, n_elements(diff) - 1L do begin
    if (diff[i] eq 1L) then begin
      if (~in_range) then r += '-'
      in_range = 1B
    endif else begin
      if (in_range) then r += string(lines[i], format=f)
      in_range = 0B
      r += ', ' + string(lines[i + 1], format=f)
    endelse
  endfor
  if (in_range) then r += string(lines[i], format=f)

  return, r
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

  ; clear code coverage (only do this if running on IDL 8.4+)
  if (mg_idlversion(require='8.4')) then begin
    for i = 0L, n_elements(*self.testing_routines) - 1L do begin
      r = (*self.testing_routines)[i]
      mg_resolve_routine, r.name, is_function=r.is_function, $
                          resolved=resolved
      if (~resolved) then begin
        printf, -2, r.name, r.is_function ? 'function' : 'procedure', $
                format='(%"could not resolve %s (%s)")'
      endif else begin
        r.resolved = 1B
        (*self.testing_routines)[i] = r
        dummy = code_coverage(r.name, $
                              function=r.is_function, $
                              /clear)
      endelse
    endfor
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
      result = self->runTest((*self.testnames)[t], message=msg, $
                             has_output=(*self.have_output)[t], output=output)
      (*self.output)[t] = ((*self.have_output)[t] && size(output, /type) ne 0L) ? strtrim(output, 2) : ''
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
    (*self.skips)[t] = self.skipped

    if (~self.failuresOnly) then begin
      self.testRunner->reportTestResult, logMsg, passed=passed, output=(*self.output)[t], $
                                         skipped=self.skipped, $
                                         time=self.time, level=self.level, $
                                         math_errors=self.math_errors
    endif
  endfor

  ; report code coverage (only do this if running on IDL 8.4+)
  if (mg_idlversion(require='8.4')) then begin
    covered_routines = !null
    self.total_nlines = 0L
    self.covered_nlines = 0L
    for i = 0L, n_elements(*self.testing_routines) - 1L do begin
      r = (*self.testing_routines)[i]
      if (r.resolved) then begin
        untested_lines = code_coverage(r.name, $
                                       function=r.is_function, nlines=nlines)
        self.total_nlines += nlines
        if (size(untested_lines, /n_dimensions) eq 0) then begin
          covered_routines = [covered_routines, r.name]
          self.covered_nlines += nlines
        endif else begin
          self.covered_nlines += nlines - n_elements(untested_lines)
        endelse
        r.untested_lines = self->_combineLines(untested_lines)
        (*self.testing_routines)[i] = r
      endif
    endfor
    if (n_elements(*self.testing_routines) gt 0L) then begin
      self.testRunner->reportTestCaseCoverage, covered_routines, $
                                               *self.testing_routines, $
                                               level=self.level, $
                                               total_nlines=self.total_nlines, $
                                               covered_nlines=self.covered_nlines
    endif
  endif

  if (~self.failuresOnly) then begin
    self.testRunner->reportTestCaseResult, npass=self.npass, $
                                           nfail=self.nfail, $
                                           nskip=self.nskip, $
                                           level=self.level
  endif
end


;= configuring a test case

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
; Find the name and number of tests (i.e. methods with names that start with
; "test") for a given class name.
;
; :Private:
;
; :Returns:
;   `strarr`
;
; :Params:
;   classname : in, required, type=string
;     unit test classname
;
; :Keywords:
;   ntests : out, optional, type=long
;     set to a named variable to return the number of test methods found
;   have_output : out, optional, type=`bytarr`
;     set to a named variable to return whether the corresponding methods from
;     the returned string array return output
;-
function mguttestcase::findTestnamesForClass, classname, $
                                              ntests=ntests, $
                                              have_output=have_output
  compile_opt strictarr

  ; find tests: any method with name test*
  help, /routines, output=routines
  functionsPos = where(strmatch(routines, 'Compiled Functions:'), count)
  routines = routines[functionsPos:*]
  result = stregex(routines, '^' + classname + '::(test[^ ]*).*', $
                   /extract, /subexpr, /fold_case)
  testnames = reform(result[1, *])

  ; find names that matched
  ind = where(testnames ne '', ntests)
  if (ntests gt 0) then begin
    testnames = testnames[ind]
  endif else testnames = ''

  have_output = bytarr(n_elements(testnames))
  for t = 0L, ntests - 1L do begin
    params = routine_info(classname + '::' + testnames[t], $
                          /parameters, /functions)
    if (params.num_kw_args eq 0L) then continue
    ind = where(params.kw_args eq 'OUTPUT', ncount)
    have_output[t] = ncount gt 0L
  endfor

  return, testnames
end


;+
; Find the name and number of tests (i.e. methods with names that start with
; "test"), including inherited ones.
;
; :Private:
;-
pro mguttestcase::findTestnames
  compile_opt strictarr

  testnames = self->findTestnamesForClass(obj_class(self), $
                                          ntests=ntests, $
                                          have_output=have_output)

  superclassnames = list(obj_class(self, count=nsuperclasses, /superclass), /extract)
  
  foreach class, superclassnames do begin
    super = obj_class(class, count=nsuper, /superclass)
    if (nsuper gt 0) then begin
      superclassnames.Add, super, /extract
      nsuperclasses += nsuper
    endif
  endforeach
  
  for s = 0L, nsuperclasses - 1L do begin
    tnames = self->findTestnamesForClass(superclassnames[s], $
                                         ntests=nsupertests, $
                                         have_output=super_output)
    if (nsupertests gt 0L) then begin
      if (ntests gt 0L) then begin
        testnames = [testnames, tnames]
        have_output = [have_output, super_output]
        ntests += nsupertests
      endif else begin
        testnames = tnames
        have_output = super_output
        ntests = nsupertests
      endelse
    endif
  endfor

  ; record results
  self.ntests = ntests
  *self.testnames = strlowcase(testnames)
  *self.have_output = have_output
end


;+
; Test suites can contain other test suites or test cases. The level is the
; number of layers down from the top most test suite (level 0).
;
; :Private:
;
; :Params:
;   level : in, required, type=integer
;     new level of object
;-
pro mguttestcase::setLevel, level
  compile_opt strictarr

  self.level = level
end


;+
; Add names of routines that are tested by a test case.
;
; :Params:
;   routine : in, required, type=string/`strarr`
;     name of routine(s) that this test case is testing
;
; :Keywords:
;   is_function : in, optional, type=boolean
;     set to indicate that `routine` contains function name(s)
;-
pro mguttestcase::addTestingRoutine, routine, is_function=is_function
  compile_opt strictarr

  for i = 0L, n_elements(routine) - 1L do begin
    if (n_elements(*self.testing_routines) eq 0L) then begin
      *self.testing_routines = { name: routine[i], is_function: keyword_set(is_function), untested_lines: '', resolved: 0B }
    endif else begin
      *self.testing_routines = [*self.testing_routines, { name: routine[i], is_function: keyword_set(is_function), untested_lines: '', resolved: 0B }]
    endelse
  endfor
end


;= property access

;+
; Get properties of the object.
;-
pro mguttestcase::getProperty, npass=npass, nfail=nfail, nskip=nskip, $
                               ntests=ntests, testnames=testnames, $
                               have_output=have_output, math_errors=math_errors, $
                               testing_routines=testing_routines, $
                               total_nlines=total_nlines, $
                               covered_nlines=covered_nlines
  compile_opt strictarr

  npass = self.npass
  nfail = self.nfail
  nskip = self.nskip
  ntests = self.ntests
  math_errors = self.math_errors
  total_nlines = self.total_nlines
  covered_nlines = self.covered_nlines

  if (arg_present(testnames)) then testnames = *self.testnames
  if (arg_present(have_output)) then have_output = *self.have_output
  if (arg_present(testing_routines)) then testing_routines = *self.testing_routines
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


;= lifecycle methods

;+
; Free resources.
;-
pro mguttestcase::cleanup
  compile_opt strictarr

  ptr_free, self.testnames, self.have_output, self.output, $
            self.logmsgs, self.passes, self.skips, self.testing_routines
end


;+
; Intialize test case.
;
; :Returns:
;   1 for succcess, 0 for failure
;
; :Keywords:
;   test_runner : in, required, type=object
;     subclass of `MGutTestRunner`
;   failures_only : in, optional, type=boolean
;     set to report only failed tests
;-
function mguttestcase::init, test_runner=testRunner, failures_only=failuresOnly
  compile_opt strictarr

  self.testRunner = testRunner
  self.failuresOnly = keyword_set(failuresOnly)

  self.testnames = ptr_new(/allocate_heap)
  self.have_output = ptr_new(/allocate_heap)
  self.output = ptr_new(/allocate_heap)
  self.logmsgs = ptr_new(/allocate_heap)
  self.passes = ptr_new(/allocate_heap)
  self.skips = ptr_new(/allocate_heap)
  self.testing_routines = ptr_new(/allocate_heap)

  self->findTestnames

  *self.output = strarr(n_elements(*self.testnames))
  *self.logmsgs = strarr(n_elements(*self.testnames))
  *self.passes = bytarr(n_elements(*self.testnames))
  *self.skips = bytarr(n_elements(*self.testnames))

  self.level = 0L

  return, 1B
end


;+
; Define member variables.
;
; :Fields:
;   testRunner
;     subclass of `MGtestRunner`
;   testnames
;     pointer to string array of method names that start with "test"
;   have_output
;     boolean array indicating whether corresponding test method has a OUTPUT
;     keyword
;   logmsgs
;     pointer to string array of log message for each test
;   passes
;     point to byte array of pass/fail status for each test
;   level
;     number of layers down from the top-containing suite
;   ntests
;     total number of tests
;   npass
;     number of passing tests
;   nfail
;     number of failing tests
;   time
;     time for the current test to run
;   failuresOnly
;     flag to indicate only failed tests should be reported
;-
pro mguttestcase__define
  compile_opt strictarr

  define = { MGutTestCase, $
             testRunner: obj_new(), $
             testnames: ptr_new(), $
             have_output: ptr_new(), $
             output: ptr_new(), $
             math_errors: 0L, $
             logmsgs: ptr_new(), $
             passes: ptr_new(), $
             skips: ptr_new(), $
             testing_routines: ptr_new(), $
             total_nlines: 0L, $
             covered_nlines: 0L, $
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

