# Carregar pacotes necessários
library(PNADcIBGE)
library(dplyr)

# PNADs
pnadc_2023_trim1 <- get_pnadc(year = 2023, quarter = 1, design = FALSE)
pnadc_2023_trim2 <- get_pnadc(year = 2023, quarter = 2, design = FALSE)
pnadc_2023_trim3 <- get_pnadc(year = 2023, quarter = 3, design = FALSE)
pnadc_2023_trim4 <- get_pnadc(year = 2023, quarter = 4, design = FALSE)
pnadc_2024_trim1 <- get_pnadc(year = 2024, quarter = 1, design = FALSE)
pnadc_2024_trim2 <- get_pnadc(year = 2024, quarter = 2, design = FALSE)

# dfs
variaveis_2023_trim1 <- names(pnadc_2023_trim1)
variaveis_2023_trim2 <- names(pnadc_2023_trim2)
variaveis_2023_trim3 <- names(pnadc_2023_trim3)
variaveis_2023_trim4 <- names(pnadc_2023_trim4)
variaveis_2024_trim1 <- names(pnadc_2024_trim1)
variaveis_2024_trim2 <- names(pnadc_2024_trim2)

# Comparar
dif_2023_2024_T1 <- setdiff(variaveis_2023_trim1, variaveis_2024_trim1)
dif_2024_2023_T1 <- setdiff(variaveis_2024_trim1, variaveis_2023_trim1)

dif_2023_2024_T2 <- setdiff(variaveis_2023_trim2, variaveis_2024_trim2)
dif_2024_2023_T2 <- setdiff(variaveis_2024_trim2, variaveis_2023_trim2)

dif_2023_T2_T3 <- setdiff(variaveis_2023_trim2, variaveis_2023_trim3)


# Diferenças
cat('Variáveis presentes em 2023 T1, mas ausentes em 2024 T1:\n')
print(dif_2023_2024_T1)

cat('\nVariáveis presentes em 2024 T1, mas ausentes em 2023 T1:\n')
print(dif_2024_2023_T1)

cat('\nVariáveis diferentes entre os trimestres 2023 T1:\n')
print(dif_2023_T1_T2_T3_T4)

## não identifique diferenças entre os semestres
