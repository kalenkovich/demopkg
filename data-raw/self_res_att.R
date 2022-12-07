## code to prepare `self_res_att` dataset goes here

library(readr)

ct <- cols(
  id = col_character(),
  sex = col_factor(levels = c("female", "male")),
  ethgroup = col_factor(),
  age = col_double(),
  m_non = col_double(),
  f_non = col_double(),
  m_self = col_double(),
  f_self = col_double(),
  grpsize = col_double(),
  group = col_double(),
  mascpref = col_double(),
  obro = col_double(),
  osis = col_double(),
  ybro = col_double(),
  ysis = col_double(),
  birthorder = col_factor(levels = c("only", "firstborn", "middleborn", "lastborn"))
)

self_res_att <- readr::read_csv("data-raw/DeBruine_2004_PRSLB_att.csv",
                                col_types = ct)

usethis::use_data(self_res_att, overwrite = TRUE)
