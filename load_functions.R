fncs <- new.env()

function_files <- list.files("functions/", full.names = TRUE)

for (file in function_files) {
  tryCatch({
    source(file, local = fncs)
  }, error = function(e) {
    message("some function files are sus: ", file, "\n", e)
  })
}

rm(function_files)