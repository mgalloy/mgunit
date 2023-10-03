; docformat = 'rst'

;+
; Create an expression to send to `lambda` that returns a long between
; `lower_bound` and `upper_bound`, inclusively.
;
; :Returns:
;   string
;
; :Params:
;   lower_bound : in, required, type=long
;     minimum allowable value
;   upper_bound : in, required, type=long
;     maximum allowable value
;-
function mgunit_long_bounded, lower_bound, upper_bound
  compile_opt strictarr

  range = upper_bound - lower_bound
  overshoot = 0.1
  over_lower_bound = lower_bound - overshoot * range
  over_upper_bound = upper_bound + overshoot * range
  expr = string(lower_bound, $
                over_upper_bound, $
                over_lower_bound, $
                over_lower_bound, $
                upper_bound, $
      format='x: %d > long((randomu(seed, 1))[0] * (%d + 1L - (%d)) + (%d)) < %d')
  return, expr
end


function mgunit_long_lowerbound, lower_bound
  compile_opt strictarr

end


function mgunit_long_upperbound, upper_bound
  compile_opt strictarr

end


function mgunit_long_nobounds
  compile_opt strictarr

  min_value = -2L^31
  max_value = 2L^31 - 1L
  expr = string(min_value, max_value, max_value, $
    format='x: %d > (long((randomn(seed, 1))[0] * %d)) < %d')

  return, expr
end


function mgunit_long, min_value=min_value, $
                      max_value=max_value
  compile_opt strictarr

  case 1 of
    n_elements(min_value) gt 0L && n_elements(max_value) gt 0L: $
      expr = mgunit_long_bounded(min_value, max_value)
    n_elements(min_value) gt 0L: expr = mgunit_long_lowerbound(min_value)
    n_elements(max_value) gt 0L: expr = mgunit_long_upperbound(max_value)
    else: expr = mgunit_long_nobounds()
  endcase

  return, lambda(expr)
end


; main-level example program

min_value = -2L^31
max_value = 2L^31 - 1L

n = 100000
t = lonarr(n)
f = mgunit_long()
for i = 0L, n - 1L do t[i] = f()
h = histogram(float(t), nbins=100, locations=locs)
window, /free, $
        title=string(n, $
                     format='%d calls to function returned from mgunit_long()')
plot, locs[0:*], h[0:*], xstyle=1, xrange=[min_value, max_value]

min_value = 0
max_value = 10

n = 100000
t = lonarr(n)
f = mgunit_long(min_value=min_value, max_value=max_value)
for i = 0L, n - 1L do t[i] = f()
h = histogram(float(t), nbins=1000, locations=locs)
window, /free, $
        title=string(n, $
                     format='%d calls to function returned from mgunit_long(min_value=0, max_value=10)')
plot, locs, h, xstyle=1, xrange=[min_value - 1L, max_value + 1L]

end
