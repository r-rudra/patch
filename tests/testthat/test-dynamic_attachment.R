test_that("dynamic_attachment works", {
  expect_output(
    patch_function(),
    "automatically cleared",
  )


  expect_true("patch:dynamic_attachment" %in% search())

  # also test of global function modification
  options(store_function_before_patch = NULL)

  env <- pos.to.env(which(search()=="patch:dynamic_attachment"))

  expect_equal(
    sum(unlist(env$locate_section(env$get_section, "tloc", all = TRUE))),
    24
  )

  assign("lsec", env$locate_section, envir = globalenv())

  patch_function(lsec, "all", TRUE, replace_it = TRUE,
                 auto_assign_patched_function = TRUE)

  expect_equal(
    sum(unlist(lsec(env$get_section, "tloc", all = TRUE))),
    sum(unlist(lsec(env$get_section, "tloc", all = FALSE)))
  )

  rm(lsec, envir = globalenv())

  expect_failure(expect_output(
    patch_function(),
    "automatically cleared",
  ))

  capture_output(patch_function())

  # https://github.com/r-lib/covr/issues/40
  .onUnload()

  expect_false("patch:dynamic_attachment" %in% search())

})
