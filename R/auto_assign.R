


auto_assign <- function(f, new_f_body, env = environment()){
  f_info <- details_of_function(f, env)

  if(isTRUE(f_info$auto_assign_possible)){
    f_info$assign_back(new_f_body)
  }else{
    stop("<auto_assign> failed", call. = FALSE)
  }

  invisible(0)

}
