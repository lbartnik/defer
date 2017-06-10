library(dplyr)
library(RSclient)

iris %>%
   group_by(Species) %>%
   parallelize %>%
   do({
     # something that's better run in paralle
     data.frame(result = 1)
   })

system.time({
  data.frame(g = sample.int(1e1, 1e7, replace = TRUE),
             r = sample(2, 1e7, replace = TRUE) - 1,
             x = rnorm(1e7), y = rnorm(1e7)) %>%
  group_by(g) %>%
  do({
    m <- glm(r~x+y, family = binomial, ., control = list(maxit = 100))
    data.frame(err = sqrt(mean(residuals(m)^2)))
  })
})

system.time({
  data.frame(g = sample.int(1e1, 1e7, replace = TRUE),
             r = sample(2, 1e7, replace = TRUE) - 1,
             x = rnorm(1e7), y = rnorm(1e7)) %>%
    group_by(g) %>%
    parallelize %>%
    do({
      m <- glm(r~x+y, family = binomial, ., control = list(maxit = 100))
      data.frame(err = sqrt(mean(residuals(m)^2)))
    })
})

library(httr)
library(defer)
wrapper <- defer(function(x)x*x)

public_opencpu_url <- "https://cloud.opencpu.org/ocpu/library/base/R/source/print"

# we're still using the same wrapper object as above
serialized_wrapper <- jsonlite::base64_enc(serialize(wrapper, NULL))

local_script_path <- tempfile(fileext = ".R")
cat(paste0("wrapper <- unserialize(jsonlite::base64_dec('", serialized_wrapper, "'))\n",
           "wrapper(10)\n"),
    file = local_script_path)

http_result <- httr::POST(public_opencpu_url,
                          body = list(file = upload_file(local_script_path)))
content(http_result, 'text')
#> [1] "$value\n[1] 29688.67\n\n$visible\n[1] TRUE\n\n"


