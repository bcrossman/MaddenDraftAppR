## code to prepare `required_depth` dataset goes here
library(tibble)
required_depth <- 
  tibble(
    position = c("QB", "RG", "LG", "C", "LT", "RT", "LE", "RE", "DT", "LOLB",
                 "ROLB", "MLB", "SS", "FS", "CB", "WR", "TE", "HB", "K", "P"),
    num = c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,1,1,1,1)
  )
usethis::use_data(required_depth, overwrite = TRUE)
