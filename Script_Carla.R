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
  mutate(across(-Nombre, ~ ifelse(.x == "N.D.", 0, .x))) #todos los N.D, son 0

library(tidyverse)
library(dplyr)



  library(tidyverse)

toxinasCarla1 <- toxinas2 %>%
  rename(Name = Nombre) %>%          # Renombramos la columna
  # Corrección específica para la segunda aparición
  mutate(Name = if_else(Name == "E1-14-6h", "E1-15", Name)) %>%
  # Extraemos Group y creamos Day y Hour según reglas
  mutate(
    Group = str_extract(Name, "^[A-E]1"),
    
    # Horas
    Hours = case_when(
      str_detect(Name, "-0$")        ~ 0L,
      str_detect(Name, "-1$")        ~ 24L,
      str_detect(Name, "-15$")       ~ 24L,
      str_detect(Name, "-\\d+h$")    ~ as.integer(str_extract(Name, "(?<=-)\\d+(?=h)")),
      TRUE                            ~ NA_integer_
    ),
    
    # Días
    Days = case_when(
      str_detect(Name, "-0$")        ~ 0L,
      str_detect(Name, "-1$")        ~ 1L,
      str_detect(Name, "-15$")       ~ 15L,
      str_detect(Name, "-14-6h$")    ~ 15L,   # caso especial E1-15
      str_detect(Name, "-\\d+h$")    ~ 0L,    # horas 0-6 ahora son día 0
      TRUE                            ~ NA_integer_
    ),
    
    # Tiempo total en horas
    Time_hours = Days * 24 + Hours
  ) %>%
  # Reordenamos columnas para que los tratamientos estén al inicio
  select(Name, Group, Days, Hours, Time_hours, everything())


###Gráficos

library(dplyr)
glimpse (toxinas5)

