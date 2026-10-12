# The summaries that the functions show in the console are tested in test-output.R; the other
# tests run without them
old_show_data <- options(faofishingreport.show_data = FALSE)
withr::defer(options(old_show_data), teardown_env())
