library(tidyverse)
toxinas <- read_delim(file = "datostoxinas.csv", delim = ";")

toxinas <- read_delim(
  file = "datostoxinas.csv",
  delim = ";",
  col_type = list(Nombre = "f"),
)

toxinas

str(toxinas)

summary(toxinas)

toxinas2 <- toxinas |>
  mutate(across(everything(), ~ ifelse(.x == "N.D.", 0, .x))) #todos los N.D, son 0

