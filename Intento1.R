library(tidyverse)
library(dplyr)
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

#Entonces [^0-9.]+ significa:

#“uno o más caracteres que no sean dígitos (0–9) ni punto (.)”

#Reemplazo: "" (cadena vacía) → lo borra.
#gsub() = “global substitute” → busca un patrón y lo reemplaza en todas las filas.

toxinas3 <- toxinas2 %>%
  mutate(MCLR = as.numeric(gsub("[^0-9.]+", "", MCLR))) 

#convertir los datos <al LOQ a 0

#grepl("^<", columna);grepl() revisa cada valor de la columna y devuelve TRUE/FALSE.

#El patrón ^< significa:  ^ = “al inicio del texto”, < = el símbolo “menor que”

#Entonces detecta valores que empiezan con <, como "<0.040".


toxinas3 <- toxinas3 |>
  mutate(`D-MCRR + DE-MCRR ` = ifelse(grepl("^<", `D-MCRR + DE-MCRR `),
                                     "0",
                                     `D-MCRR + DE-MCRR `)) |>
  mutate(`D-MCRR + DE-MCRR ` = as.numeric(`D-MCRR + DE-MCRR `))
names(toxinas3)

#same but applied for all col
library(dplyr)
# cortar la tabla 

toxinas3 <- toxinas3 |>
  mutate(across(where(is.character),
                ~ as.numeric(ifelse(grepl("^<", .x), "0", .x))))
str(toxinas3)
toxinas4 <- toxinas3 |> 
  slice(1:40)
#separar e añadir columnas 
toxinas5 <- toxinas4 %>%
  
  rename(Name = Nombre) %>%
  
  group_by(Name) %>%
  
  mutate(
    
    dup_index = row_number()
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    # Corrección nominal SOLO para la segunda aparición
    
    Name = if_else(Name == "E1-14-6h" & dup_index == 2, "E1-15", Name),
    
    Group = str_extract(Name, "^[A-E]1"),
    
    Hours = case_when(
      
      Name == "E1-15"                ~ 0L,
      
      str_detect(Name, "-(\\d+)h")   ~ as.integer(str_extract(Name, "(?<=-)\\d+(?=h)")),
      
      str_detect(Name, "-0$|-1$|-15$") ~ 0L,
      
      TRUE                           ~ NA_integer_
      
    ),
    
    Days = case_when(
      
      Name == "E1-15"                ~ 15L,
      
      str_detect(Name, "-0$")        ~ 0L,
      
      str_detect(Name, "-14-")       ~ 14L,
      
      str_detect(Name, "-15$")       ~ 15L,
      
      str_detect(Name, "-1$")        ~ 1L,
      
      str_detect(Name, "-(\\d+)h")   ~ 1L,
      
      TRUE                           ~ NA_integer_
      
    )
    
  ) %>%
  
  select(Name, Group, Days, Hours, everything(), -dup_index)
