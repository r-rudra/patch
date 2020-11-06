
.onUnload <- function(libpath) {
  if (is_dynamic_attachment_done()) {
    clear_dynamic_attachment()
    # Sys.sleep required to clear attachment properly at exit
    # otherwise "R symbol not found" error is coming
    Sys.sleep(0.001)
  }
}
