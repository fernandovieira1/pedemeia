# Limpar o ambiente
rm(list=ls(all=TRUE)); gc(); cat('\014')

#### CONFIGURAR AMBIENTE ####

### Definir período ####
# AVISO: Definir anos inicial e final
# AVISO: A análise da evasão escolar sempre requer mais de um ano e a inclusão do 1º semestre.
anos <- c(2021, 2022, 2023, 2024)
trimestres <- c(1)

### Local de trabalho ####
# AVISO: Verifique abaixo o caminho do arquivo e altere-o (se ainda não o fez)
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'
dir(local)

### Carregar script de configuração do ambiente ####
# AVISO: Não mexer
source('Codigo Novo pdm\\Script pdm\\0_config.R')

glimpse(publico_alvo_filtrado)
table(publico_alvo_filtrado$Ano)
table(publico_alvo_filtrado$Trimestre)
