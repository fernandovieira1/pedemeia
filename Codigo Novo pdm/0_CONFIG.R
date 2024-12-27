# Limpar o ambiente
rm(list=ls(all=TRUE)); gc(); cat('\014')

#### CONFIGURAR AMBIENTE ####

### Definir período ####
# AVISO: Definir anos inicial e final
# AVISO: A análise da evasão escolar sempre requer mais de um ano e a inclusão do 1º semestre.
anos <- c(2022, 2023)
trimestres <- c(1, 2, 3, 4)

### Local de trabalho ####
# AVISO: Verifique abaixo o caminho do arquivo e altere-o (se ainda não o fez)
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'

### Carregar script de configuração do ambiente ####
# AVISO: Não mexer
source('Codigo Novo pdm\\Script pdm\\0_config.R')
