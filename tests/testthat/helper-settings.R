# Restores the default settings at the end of a test, with the summaries in the console still off
# (setup-output.R)
defer_reset_settings <- function(env = parent.frame()) {
  withr::defer(fao_settings(reset = TRUE, show_data = FALSE), envir = env)
}
