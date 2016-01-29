Jobloop.job_loop (int_of_string Sys.argv.(2))
  (fun line -> ignore (Sys.command (Sys.argv.(1) ^ line)))


