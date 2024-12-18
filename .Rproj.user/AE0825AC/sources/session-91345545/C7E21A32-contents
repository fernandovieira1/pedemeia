### VALIDAÇÃO AED - Validação e Ajustes ###

# Validar colunas carregadas
expected_cols <- c('ID_DOMICILIO', 'UPA', 'V1008', 'V1014', 'V2003', 'V2009', 'V3003A', 'VD4020')
missing_cols <- setdiff(expected_cols, names(dados_pnad))
if (length(missing_cols) > 0) {
  stop(paste("Faltando colunas:", paste(missing_cols, collapse = ", ")))
}

# Validar categorias esperadas
validate_categories <- function(df, col, valid_values) {
  invalid <- setdiff(unique(df[[col]]), valid_values)
  if (length(invalid) > 0) {
    warning(paste("Valores inesperados em", col, ":", paste(invalid, collapse = ", ")))
  }
}
validate_categories(dados_pnad, "V3003A", c("Regular do ensino médio", "EJA"))
validate_categories(dados_pnad, "V3002A", c("Rede pública", "Rede privada"))

# Ajustar criação de variáveis
base_evasao <- base_evasao %>%
  mutate(
    ensino_medio = ifelse(
      (V2009 >= 14 & V2009 <= 24 & 
         V3002A == 'Rede pública' & 
         V3003A == 'Regular do ensino médio'),
      1, 0
    ),
    evasao = ifelse(
      Trimestre == 1 & V3003A == 'Regular do ensino médio' &
        !(dplyr::lead(Trimestre, default = NA_integer_) == 1 & 
            dplyr::lead(Ano, default = NA_integer_) == Ano + 1 & 
            dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio'),
      1, 0
    )
  )

# Ajustar resumo de evasão e abandono
base_evasao %>%
  group_by(Ano, Trimestre) %>%
  summarise(
    Total = n(),
    Evasao = sum(evasao, na.rm = TRUE),
    Proporcao_Evasao = mean(evasao, na.rm = TRUE)
  ) %>%
  print()

base_abandono %>%
  group_by(Ano, Trimestre) %>%
  summarise(
    Total = n(),
    Abandono = sum(abandono, na.rm = TRUE),
    Proporcao_Abandono = mean(abandono, na.rm = TRUE)
  ) %>%
  print()
