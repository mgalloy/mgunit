; docformat = 'rst'

;+
; Returns a string message indicating the errors denoted by the given
; `CHECK_MATH`-type `bitmask`.
;
; :Returns:
;   string
;
; :Params:
;   bitmask : in, required, type=integer
;     bitmask as from result of `CHECK_MATH`
;-
function mg_math_message, bitmask
  compile_opt strictarr

  ntypes = 6L
  errors = bytarr(ntypes)
  codes = [1L, 2L, 16L, 32L, 64L, 128L]
  msgs = ['integer divided by zero', $
          'integer overflow', $
          'floating-point divided by zero', $
          'floating-point underflow', $
          'floating-point overflow', $
          'floating-point operand error']

  for i = 0L, ntypes - 1L do begin
    errors[i] = (bitmask and codes[i]) gt 0L
  endfor

  ind = where(errors, nerrors)

  return, nerrors eq 0L ? '' : strjoin(msgs[ind], ', ')
end
