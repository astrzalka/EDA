## code to prepare `my_dataset` dataset goes here

przyklad <- read.table('data-raw/przyklad.txt', header=TRUE, fill = TRUE, sep = ' ', quote = "\"")
usethis::use_data(przyklad, overwrite = TRUE)
