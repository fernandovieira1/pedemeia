# Limpar o ambiente
rm(list=ls(all=TRUE)); gc(); cat('\014')

#### CONFIGURAR AMBIENTE ####

### Definir período ####
# AVISO: Definir anos inicial e final
anos <- c(2022, 2023)

### Local de trabalho ####
# AVISO: Verifique abaixo o caminho do arquivo e altere-o (se ainda não o fez)
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'

### Carregar script de configuração do ambiente ####
# AVISO: Não mexer
source('Codigo Novo pdm\\Script pdm\\0_config_amb.R')
