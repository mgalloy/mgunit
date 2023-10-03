; docformat = 'rst'

function mgunit_float, min_value=min_value, $
                       max_value=max_value, $
                       allow_nan=allow_nan
  compile_opt strictarr

  return, lambda(x:(randomu(seed, 1))[0])
end
