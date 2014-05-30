-record(running_avg, {
          avg = 0, var = 0, std = 0,
          itteration = 0, dist = 0.0,
          warn = 0, info = 0, error = 0,
          decay = 0.9999, quick_decay = 0.99,
          min_iter = 1000,
          t_error,t_warn,t_info
         }).
