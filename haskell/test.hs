

fixed_point f val epsilon =
    let next_val = f val in
      if abs(val - next_val) <= epsilon then
        val
      else
        fixed_point f next_val epsilon



log_fixed_point_fn val =
  let fn x =
        exp(x) - val + x in
  fn

mylog x =
  fixed_point (log_fixed_point_fn x) (x / 2) 1e-5

