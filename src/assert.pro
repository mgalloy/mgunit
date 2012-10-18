; docformat = 'rst'

;+
; Raises an error if the given condition is not met. Uses `logical_predicate`
; to determine truth of condition: so zero or null values are false, anything
; else is true. Be careful of conditions like the following::
;     
;    assert, not file_test(filename)
;
; This uses the bitwise `not` operator and therefore this assertation is 
; always false.
;
; `ASSERT` clears math errors to eliminate messages about math errors in 
; `MGUNIT` output.
;
; :Examples:
;    It is typical to check the error in a calculation like the following::
;
;       assert, error gt tolerance, 'incorrect result, error = %f', error
;
;    It is also useful to check a pre-condition for running a test at the 
;    beginning of the test and skip the test if not met::
;
;       assert, long((strsplit(!version.release, ',', /extract))[0]) ge 8, $
;               'IDL version too old: %s', !version.release, $
;               /skip
;
; :Params: 
;    condition : in, required, type=boolean
;       condition to assert
;    msg : in, optional, type=string, default="'Assertion failed'"
;       message to throw if condition is not met
;    arg1 : in, optional, type=string
;       first argument for any C format codes in msg
;    arg2 : in, optional, type=string
;       second argument for any C format codes in msg
;    arg3 : in, optional, type=string
;       third argument for any C format codes in msg
;
; :Keywords:
;    skip : in, optional, type=boolean
;       set to skip the current test instead of passing or failing
;-
pro assert, condition, msg, arg1, arg2, arg3, skip=skip
  compile_opt strictarr, logical_predicate, hidden
  on_error, 2

  if (~condition) then begin
    null = check_math()
    if (keyword_set(skip)) then (scope_varfetch('self', level=-1))->skip

    case n_params() of
      0: 
      1: message, 'Assertion failed'
      2: message, msg
      3: message, string(arg1, format='(%"' + msg + '")')
      4: message, string(arg1, arg2, format='(%"' + msg + '")')
      5: message, string(arg1, arg2, arg3, format='(%"' + msg + '")')
    endcase
  endif
end
