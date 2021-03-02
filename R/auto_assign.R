


auto_assign <- function(f,
                        new_f_body, new_f_arg,
                        new_f_arg_integrate,
                        env = environment()){
  f_info <- details_of_function(f, env)

  if(isTRUE(f_info$auto_assign_possible)){
    f_info$assign_back(new_f_body, new_f_arg, new_f_arg_integrate)
  }else{
    stop("<auto_assign> failed", call. = FALSE)
  }

  invisible(0)

}
