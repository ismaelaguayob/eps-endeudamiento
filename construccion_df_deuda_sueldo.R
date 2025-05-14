# dataframe de análisis combinado (2020 y 2024)

library(pacman)
pacman::p_load(
  dplyr,      
  haven,      
  tidyr,      
  purrr,      
  ggplot2     
)

options(scipen = 999)
# rm(list = ls())

# Rutas 2020
path_modulo_c_2020 <- "./datos-2020/MODULO_C_Entrevistado_in.dta"
path_modulo_d_2020 <- "./datos-2020/MODULO_D_Entrevistado_in.dta"

# Rutas 2024
path_modulo_c_2024 <- "./datos-2024/MODULO_C_ENTREVISTADO.dta"
path_modulo_d_2024 <- "./datos-2024/MODULO_D_ENTREVISTADO.dta"

load_data <- function(path) {
  df <- read_stata(path)
  return(df)
}

# Cargar datos 2020
modulo_c_2020 <- NULL
modulo_d_2020 <- NULL
if (file.exists(path_modulo_c_2020)) {
  modulo_c_2020 <- load_data(path_modulo_c_2020)
}
if (file.exists(path_modulo_d_2020)) {
  modulo_d_2020 <- load_data(path_modulo_d_2020)
}

# Cargar datos 2024
modulo_c_2024 <- NULL
modulo_d_2024 <- NULL
if (file.exists(path_modulo_c_2024)) {
  modulo_c_2024 <- load_data(path_modulo_c_2024)
}
if (file.exists(path_modulo_d_2024)) {
  modulo_d_2024 <- load_data(path_modulo_d_2024)
}

# Definición de variables clave ---
identificador_folio <- "folio_n20"

# Variables d37m para 2020 (deuda mensuales del hogar)
vars_d37m_2020 <- c(
  "d37m_2_ed", "d37m_3_ed", "d37m_4_ed", "d37m_5_ed", "d37m_6_ed", 
  "d37m_7_ed", "d37m_8_ed", "d37m_9_ed", "d37m_10_ed", "d37m_11_ed", "d37m_12_ed"
)

# Variables d37m para 2024 (deuda mensuales del hogar)
vars_d37m_2024 <- c(
  "d37m_2", "d37m_3", "d37m_5", "d37m_6", "d37m_7", 
  "d37m_8", "d37m_9", "d37m_10", "d37m_11", "d37m_12"
)

# Variable c34 (ingreso)
var_c34_2020 <- "c34_ed"
var_c34_2024 <- "c34"

# Procesamiento de datos 2020 ---
df_2020_final <- NULL

if (!is.null(modulo_c_2020) && !is.null(modulo_d_2020)) {
  # Procesar Módulo C 2020
  df_c_2020_proc <- modulo_c_2020 %>%
    select(all_of(identificador_folio), ingreso_c34_2020 = all_of(var_c34_2020)) %>%
    mutate(ingreso_c34_2020 = ifelse(ingreso_c34_2020 %in% c(8, 9), NA_real_, as.numeric(ingreso_c34_2020))) %>%
    mutate(ingreso_c34_anual_2020 = ingreso_c34_2020 * 12)
  
  # Procesar Módulo D 2020
  df_d_2020_proc <- modulo_d_2020 %>%
    select(all_of(identificador_folio), any_of(vars_d37m_2020)) %>%
    mutate(across(any_of(vars_d37m_2020), ~ifelse(. %in% c(8, 9), NA_real_, .))) %>%
    mutate(across(any_of(vars_d37m_2020), ~replace_na(as.numeric(.), 0))) %>%
    mutate(suma_deuda_d37m_2020 = rowSums(select(., any_of(vars_d37m_2020)), na.rm = TRUE)) %>%
    select(all_of(identificador_folio), suma_deuda_d37m_2020)

  # Unir datos procesados de 2020
  df_2020_final <- full_join(df_c_2020_proc, df_d_2020_proc, by = identificador_folio)
}

# Procesamiento de datos 2024 ---
df_2024_final <- NULL

if (!is.null(modulo_c_2024) && !is.null(modulo_d_2024)) {
  # Procesar Módulo C 2024
  df_c_2024_proc <- modulo_c_2024 %>%
    select(all_of(identificador_folio), ingreso_c34_2024 = all_of(var_c34_2024)) %>%
    mutate(ingreso_c34_2024 = ifelse(ingreso_c34_2024 %in% c(8, 9), NA_real_, as.numeric(ingreso_c34_2024))) %>%
    mutate(ingreso_c34_anual_2024 = ingreso_c34_2024 * 12)
  
  # Procesar Módulo D 2024
  df_d_2024_proc <- modulo_d_2024 %>%
    select(all_of(identificador_folio), any_of(vars_d37m_2024)) %>%
    mutate(across(any_of(vars_d37m_2024), ~ifelse(. %in% c(8, 9), NA_real_, .))) %>%
    mutate(across(any_of(vars_d37m_2024), ~replace_na(as.numeric(.), 0))) %>%
    mutate(suma_deuda_d37m_2024 = rowSums(select(., any_of(vars_d37m_2024)), na.rm = TRUE)) %>%
    select(all_of(identificador_folio), suma_deuda_d37m_2024)
  
  # Unir datos procesados de 2024
  df_2024_final <- full_join(df_c_2024_proc, df_d_2024_proc, by = identificador_folio)
}

# --- Unir dataframes de 2020 y 2024 --- 
df_analisis_combinado <- NULL

if (!is.null(df_2020_final) && !is.null(df_2024_final) && nrow(df_2020_final) > 0 && nrow(df_2024_final) > 0) {
  df_analisis_combinado <- inner_join(df_2020_final, df_2024_final, by = identificador_folio)
  if (nrow(df_analisis_combinado) == 0) {
    df_analisis_combinado <- NULL # Si no hay folios comunes, el resultado es NULL
  }
}

# El dataframe final se llama 'df_analisis_combinado'.
#TODO:
# Visualización (gráficos, estadísticas descriptivas, etc.)
# Calcular ratio deuda/sueldo
# Calcular suma_deuda_d37m_2020/suma_deuda_d37m_2024 solo si d37_a es "Si" (1)