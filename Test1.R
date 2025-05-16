#

sum(is.na(df_analisis_combinado$suma_deuda_d37m_2024))
sum(is.na(modulo_d_2024$d37m_6))

vars_d37m_2024 <- c(
  "d37m_2", "d37m_3", "d37m_5", "d37m_6", "d37m_7", 
  "d37m_8", "d37m_9", "d37m_10", "d37m_11", "d37m_12"
)

modulo_d_2024_NA <- modulo_d_2024 %>%
  mutate(across(all_of(vars_d37m_2024), 
                ~ ifelse(.x %in% c(8, 9), NA, .x)))

modulo_d_2024_NA %>%
  filter(if_all(all_of(vars_d37m_2024), is.na)) %>%
  nrow()

vars_d37m_2020 <- c(
  "d37m_2_ed", "d37m_3_ed", "d37m_4_ed", "d37m_5_ed", "d37m_6_ed", 
  "d37m_7_ed", "d37m_8_ed", "d37m_9_ed", "d37m_10_ed", "d37m_11_ed", "d37m_12_ed"
)

modulo_d_2020_NA <- modulo_d_2020 %>%
  mutate(across(all_of(vars_d37m_2020), 
                ~ ifelse(.x %in% c(8, 9), NA, .x)))

modulo_d_2020_NA %>%
  filter(if_all(all_of(vars_d37m_2020), is.na)) %>%
  nrow()

folios_no_na_2024 <- modulo_d_2024_NA %>%
  filter(
    !if_all(all_of(vars_d37m_2024), is.na) # La clave es el '!' al principio
  ) %>%
  pull(folio_n20) %>%
  unique()

folios_no_na_2020 <- modulo_d_2020_NA %>%
  filter(
    !if_all(all_of(vars_d37m_2020), is.na) # La clave es el '!' al principio
  ) %>%
  pull(folio_n20) %>%
  unique()

folios_comunes_no_na <- intersect(folios_no_na_2024, folios_no_na_2020)

length(folios_comunes_no_solo_na)


