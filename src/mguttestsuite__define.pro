; docformat = 'rst'

;+
; Test suites are containers for test cases. Either subclass `MGutTestSuite`
; and add test suites/test cases in its `init` method or create an
; `MGutTestSuite` and use the `add` method to add test suites/cases.
;
; For example, it is typical do create a test suite like the following::
;
;   function mytestsuite_uts::init, _extra=e
;     compile_opt strictarr
;
;     if (~self->MGutTestSuite::init(_strict_extra=e)) then return, 0
;
;     self->add, /all
;
;     return, 1
;   end
;
;   pro mytestsuite_uts__define
;     compile_opt strictarr
;
;     define = { MyTestSuite_uts, inherits MGutTestSuite }
;   end
;
; This test will add all the files in the current directory that end in
; "_ut__define.pro" as test cases. Then the following will run all the test
; cases in a directory::
;
;   IDL> mgunit, 'mytestsuite_uts'
;
; :Properties:
;   home : type=string
;     location of the root of the test suite
;   failures_only : type=boolean
;     set to report only failed tests
;   name : type=string
;     name of the object
;   npass : type=integer
;     number of passing tests contained in the hierarchy below this object
;   nfail : type=integer
;     number of failing tests contained in the hierarchy below this object
;   nskip : type=integer
;       number of skipped tests contained in the hierarchy below this object
;   ntestcases : type=integer
;     number of directly contained test suites or test cases
;   ntests : type=integer
;     number of tests contained in the hierarchy below this object
;   test_runner : in, required, type=object
;     subclass of `MGtestRunner`
;   total_nlines : type=long
;     total number of lines in testing routines
;   covered_nlines : type=long
;     number of tested lines in testing routines
;-


;= running the test suite

;+
; Recompile the class definition before creating the object to make sure it is
; the newest definition (convenient when making changes to a test).
;
; :Private:
;
; :Params:
;   classname : in, required, type=string
;     classname of class to recompile
;-
pro mguttestsuite::_recompile, classname
  compile_opt strictarr

  error = 0L
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    return
  endif

  resolve_routine, classname + '__define'
end


;+
; Create a new test case or test suite, but check for errors while creating
; it.
;
; :Private:
;
; :Returns:
;   object
;
; :Params:
;   testname : in, required, type=string
;     classname of test case or test suite to create
;
; :Keywords:
;   error : out, optional, type=boolean
;     0 if no error and 1 if an error
;-
function mguttestsuite::_makeTestCase, testname, error=error
  compile_opt strictarr
  @mgunit_common

  error = 0L
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    return, obj_new()
  endif

  self->_recompile, testName

  return, obj_new(testName, test_runner=self.testRunner, $
                  failures_only=self.failuresOnly, _extra=mgunit_common_extra)
end


;+
; Recompiles all test cases contained by the suite or contained by child
; suites.
;
; :Private:
;-
pro mguttestsuite::recompileTestCases
  compile_opt strictarr
  
  for i = 0L, self.testcases->count() - 1L do begin
    testcase = self.testcases->get(position=i)
    if (obj_isa(testcase, 'MGutTestSuite')) then begin
      testcase->recompileTestCases
    endif else begin
      self->_recompile, obj_class(testcase)
    endelse
  endfor
end


;+
; Display test results via test runner methods.
;
; :Private:
;-
pro mguttestsuite::display
  compile_opt strictarr

  if (self.nfail eq 0L) then return
   
  self->getProperty, name=name, ntestcases=ntestcases, ntests=ntests
  self.testRunner->reportTestSuiteStart, name, $
                                         ntestcases=ntestcases, $
                                         ntests=ntests, $
                                         level=self.level

  for t = 0L, ntestcases - 1L do begin
    otestcase = self.testcases->get(position=t)
    otestcase->display
  endfor

  self.testRunner->reportTestSuiteResult, npass=self.npass, $
                                          nfail=self.nfail, $
                                          nskip=self.nskip, $
                                          level=self.level, $
                                          total_nlines=self.total_nlines, $
                                          covered_nlines=self.covered_nlines
end


;+
; Run the contained test suites or test cases.
;
; :Private:
;-
pro mguttestsuite::run
  compile_opt strictarr

  self.npass = 0L
  self.nfail = 0L
  self.nskip = 0L
  
  if (~self.failuresOnly) then begin
    self->getProperty, name=name, ntestcases=ntestcases, ntests=ntests, testing_routines=testing_routines
    self.testRunner->reportTestSuiteStart, name, $
                                           ntestcases=ntestcases, $
                                           ntests=ntests, $
                                           level=self.level
  endif

  if (mg_idlversion(require='8.4')) then begin
    ; clear code coverage results from a previous run
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

  ntestcases = self.testcases->count()
  for t = 0L, ntestcases - 1L do begin
    otestcase = self.testcases->get(position=t)
    otestcase->run

    ; accumulate results
    otestcase->getProperty, npass=npass, nfail=nfail, nskip=nskip, $
                            total_nlines=total_nlines, $
                            covered_nlines=covered_nlines

    self.npass += npass
    self.nfail += nfail
    self.nskip += nskip
    self.total_nlines += total_nlines
    self.covered_nlines += covered_nlines
  endfor

  if (~self.failuresOnly) then begin
    self->getProperty, testing_routines=testing_routines
    self.testRunner->reportTestSuiteResult, npass=self.npass, $
                                            nfail=self.nfail, $
                                            nskip=self.nskip, $
                                            level=self.level, $
                                            total_nlines=self.total_nlines, $
                                            covered_nlines=self.covered_nlines, $
                                            testing_routines=testing_routines
  endif
end


;= configure test suite

;+
; Add a scalar or array of test suites or test cases.
;
; :Params:
;   tests : in, required, type=strarr
;     classnames of test suites or test cases
;
; :Keywords:
;   all : in, optional, type=boolean
;     set to add all the files in the current directory that end in
;     "_ut__define.pro" (the current directory is defined to be the
;     directory where the method calling this method is located)
;   _extra : in, optional, type=keywords
;     keywords to `MGutTestCase` subclass `init` methods
;-
pro mguttestsuite::add, tests, all=all, _extra=e
  compile_opt strictarr

  if (keyword_set(all)) then begin
    ; first, add all the unit tests in the current directory

    ; find matching files
    testFiles = file_search(self.home + '*_ut__define.pro', $
                            /fold_case, count=nTestFiles)

    ; extract just classname from each file
    myTests = file_basename(testFiles)
    for t = 0L, nTestFiles - 1L do begin
      ; "__define.pro" is 12 characters long
      myTests[t] = strmid(myTests[t], 0, strlen(myTests[t]) - 12L)
      self->add, myTests[t], _extra=e
    endfor

    ; second, for each directory in the current directory, either:
    ;   1) add the *_uts__define.pro found there, or
    ;   2) if no _uts__define.pro found, create one and add /ALL
    testDirs = file_search(self.home + '*', $
                           /test_directory, /mark_directory, $
                           count=nTestDirs)
    for d = 0L, nTestDirs - 1L do begin
      ; if there is a *_uts__define.pro here then add it
      uts = file_search(testDirs[d], '*_uts__define.pro', $
                        count=nTestSuites)
      if (nTestSuites gt 0) then begin
        suiteName = file_basename(uts[0])
        ; "__define.pro" is 12 characters long
        suiteName = strmid(suiteName, 0, strlen(suiteName) - 12L)
        if (suiteName ne self.name) then self->add, suiteName, _extra=e

        continue   ; finished with this directory
      endif

      ; if there isn't a *_uts__define.pro, but there are unit tests here
      ; then create a new test suite
      uts = file_search(testDirs[d], '*_ut__define.pro', $
                        count=nTestCases)
      if (nTestCases eq 0) then continue   ; finished with this directory

      otestsuite = obj_new('MGutTestSuite', home=testDirs[d], $
                           name=file_basename(testDirs[d]), $
                           test_runner=self.testRunner, $
                           failures_only=self.failuresOnly, $
                           _extra=e)
      otestsuite->add, /all, _extra=e
      self.testcases->add, otestsuite
    endfor
  endif else begin
    for t = 0L, n_elements(tests) - 1L do begin
      dotpos = strpos(tests[t], '.')
      if (dotpos eq -1L) then begin
        classname = tests[t]
      endif else begin
        classname = strmid(tests[t], 0, dotpos)
        methodname = strmid(tests[t], dotpos + 1L)
      endelse

      ; don't add yourself to yourself
      if (classname eq self.name) then continue

      ; see if test is valid
      otestcase = self->_makeTestCase(classname, error=error)
      if (error ne 0L) then begin
        print, 'Error creating ' + classname + ' object: ' + !error_state.msg
        continue
      endif

      ; test case is OK so now set it up
      otestcase->setLevel, self.level + 1L
      if (n_elements(methodname) gt 0L) then begin
        otestcase->setProperty, testnames=[methodname]
      endif
      self.testcases->add, otestcase
    endfor
  endelse
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
pro mguttestsuite::setLevel, level
  compile_opt strictarr

  self.level = level
  for t = 0L, self.testcases->count() - 1L do begin
    testcase = self.testcases->get(position=t)
    testcase->setLevel, level + 1
  endfor
end


;= property access

;+
; Get properties of the object.
;-
pro mguttestsuite::getProperty, name=name, $
                                npass=npass, nfail=nfail, nskip=nskip, $
                                ntestcases=ntestcases, ntests=ntests, $
                                total_nlines=total_nlines, $
                                covered_nlines=covered_nlines, $
                                testing_routines=testing_routines
  compile_opt strictarr

  name = self.name
  npass = self.npass
  nfail = self.nfail
  nskip = self.nskip
  total_nlines = self.total_nlines
  covered_nlines = self.covered_nlines
  testing_routines = *self.testing_routines

  if (arg_present(ntestcases)) then ntestcases = self.testcases->count()

  if (arg_present(ntests)) then begin
    ntests = 0L
    for t = 0L, self.testcases->count() - 1L do begin
      otestcase = self.testcases->get(position=t)
      otestcase->getProperty, ntests=nCaseTests
      ntests += nCaseTests
    endfor
  endif

end


;= lifecycle methods

;+
; Free resources.
;-
pro mguttestsuite::cleanup
  compile_opt strictarr

  obj_destroy, self.testcases
  ptr_free, self.testing_routines
end


;+
; Initialize test suite.
;
; :Returns:
;   1 for success, 0 for failure
;
; :Keywords:
;   name : in, optional, type=string, default=classname
;     name of the test suite
;   home : in, optional, type=string, default=''
;     location of the root of the test suite
;   test_runner : in, required, type=object
;     subclass of `MGutTestRunner`
;   failures_only : in, optional, type=boolean
;     set to report only failed tests
;   _extra : in, optional, type=keywords
;     keywords to `MGutTestCase` subclass `init` methods
;-
function mguttestsuite::init, name=name, $
                              home=home, $
                              test_runner=testRunner, $
                              failures_only=failuresOnly, $
                              _extra=e
  compile_opt strictarr

  self.name = n_elements(name) eq 0 ? strlowcase(obj_class(self)) : name

  if (n_elements(home) eq 0) then begin
    ; get directory of caller's source code (subtract 2L to *caller* dir)
    traceback = scope_traceback(/structure)
    callingFrame = traceback[n_elements(traceback) - 2L > 0]
    self.home = file_dirname(callingFrame.filename, /mark_directory)
  endif else begin
    self.home = strmid(home, 0, 1, /reverse_offset) eq path_sep() $
      ? home $
      : home + path_sep()
  endelse

  self.level = 0L

  self.testRunner = testRunner
  self.failuresOnly = keyword_set(failuresOnly)

  self.testcases = obj_new('IDL_Container')
  self.testing_routines = ptr_new(/allocate_heap)

  return, 1B
end

;+
; Add names of routines that are tested by a test suite.
;
; :Params:
;   routine : in, required, type=string/`strarr`
;     name of routine(s) that this test suite is testing
;
; :Keywords:
;   file_path : in, optional, type=string/`strarr`
;     contains the path to the source file for each element of routine.
;     If not specified, this information is determined by calling routine_info
;     on each routine using the is_function flag.
;   is_function : in, optional, type=boolean
;     set to indicate that `routine` contains function name(s)
;-
pro mguttestsuite::addTestingRoutine, routine, file_path=file_path, is_function=is_function
  compile_opt strictarr

  for i = 0L, n_elements(routine) - 1L do begin
    if n_elements(file_path)-1 ge i then begin
      fpath = file_path[i]
    endif else begin
      info = routine_info(routine[i], /source, functions=is_function)
      if n_elements(info) eq 0 then continue
      fpath = info[0].path
    endelse
    if (n_elements(*self.testing_routines) eq 0L) then begin
      *self.testing_routines = { name: routine[i], is_function: keyword_set(is_function), resolved: 0B, path: fpath }
    endif else begin
      *self.testing_routines = [*self.testing_routines, { name: routine[i], is_function: keyword_set(is_function), resolved: 0B, path: fpath }]
    endelse
  endfor
end

;+
; Add names of routines that are tested by a test suite.
;
; :Params:
;   folders : in, required, type=string/`strarr`
;     folder(s) to scan for functions and procedures for testing.
;     Paths can be absolute or relative. If relative, they are
;     relative to the location of the uts__define.pro (testsuite) source file,
;     or the home keyword used when the testsuite was initialized.
;-
pro mguttestsuite::addTestingFolder, folders
  compile_opt strictarr
  
  for f = 0L, n_elements(folders) do begin
    folder = folders[f]
    if strmid(folder, 0, 1) eq '/' then $
      search_path = folder $
    else $
      search_path = filepath(folder, root_dir=self.home)
    ; convert to normalized, not relative path
    full_search_path = file_expand_path(search_path)
    search_mask = filepath('*.pro', root_dir=full_search_path)
    testFiles = file_search(search_mask, /fold_case, count=nTestFiles)

    ; get just the idl file names, no paths
    routines = file_basename(testFiles, '.pro')

    ; find all the routines via the idl search path
    for i = 0, n_elements(routines)-1 do begin
      routine = routines[i]
      catch, error
      if (error ne 0L) then begin
        catch, /cancel
        ;      print, routine + " could not be resolved"
        continue
      endif
      resolve_routine, routine, /compile_full_file, /either
    endfor

    ; resolve any routines called by the found files
    resolve_all, /continue_on_error, /quiet

    ; get a listing of all the source files
    procedures = routine_info(/source)
    functions = routine_info(/source, /functions)

    ; remove any that are not under the given folder
    ind = where(file_dirname(procedures.path) eq full_search_path, nind)
    if nind gt 0 then begin
      procpaths = procedures[ind].path
      procnames = procedures[ind].name
      self->addTestingRoutine, procnames, file_path=procpaths
    endif

    ind = where(file_dirname(functions.path) eq full_search_path, nind)
    if nind gt 0 then begin
      funcpaths = functions[ind].path
      funcnames = functions[ind].name
      self->addTestingRoutine, funcnames, file_path=funcpaths, /is_function
    endif
  endfor
end

;+
; Define member variables.
;
; :Fields:
;   name
;     name of the object
;   home
;     directory (with trailing slash) containing the source code for this test
;     suite
;   level
;     number of layers below the top-most containing test suite
;   testcases
;     `IDL_Container` holding test suites or test cases
;   testRunner
;     subclass of `MGutTestRunner`
;   testing_routines
;     optional list of testing_routines shared across all tests in the test suite
;   npass
;     number of passing tests contained in the hierarchy below this test suite
;   nfail
;     number of failing tests contained in the hierarchy below this test suite
;   nskip
;     number of skipped tests contained in the hierarchy below this test suite
;   failuresOnly
;     flag to indicate only failed tests should be reported
;-
pro mguttestsuite__define
  compile_opt strictarr

  define = { MGutTestSuite, $
             name: '', $
             home: '', $
             level: 0L, $
             testcases: obj_new(), $
             testRunner: obj_new(), $
             testing_routines: ptr_new(), $
             total_nlines: 0L, $
             covered_nlines: 0L, $
             npass: 0L, $
             nfail: 0L, $
             nskip: 0L, $
             failuresOnly: 0B $
           }
end
