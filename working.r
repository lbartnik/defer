library(base64enc)

serialize_object <- function (object) {
  rds_buffer <- rawConnection(raw(0), 'w')
  on.exit(close(rds_buffer))
  saveRDS(object, rds_buffer)
  base64encode(rawConnectionValue(rds_buffer))
}

x <- serialize_object(d)
nchar(x)

temp_path <- tempfile(fileext = ".R")
temp_file <- file(temp_path, open = "w")
contents  <- paste0("x <- '", x, "'\n",
                    "library(base64enc)\n",
                    "b <- rawConnection(base64decode(x), 'r')\n",
                    "d <- readRDS(b)\n",
                    "d(b = 2)\n")
writeChar(contents, nchars = nchar(contents), temp_file, eos = NULL)
close(temp_file)


library(httr)

host <- "http://localhost:5656/ocpu"
where <- paste0(host, "/library/base/R/source/json")

result <- POST(where, body = list(file = httr::upload_file(temp_path)))
content(result, 'text')


body  <- list(file = paste0('"', basename(temp_path), '"'))
result <- POST(where, body = body)
content(result, 'text')




