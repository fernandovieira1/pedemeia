# Limpar o ambiente
rm(list=ls(all=TRUE)); gc(); cat('\014')

#### CONFIGURAR AMBIENTE ####

### Definir período ####
## AVISO: Definir anos inicial e final
## AVISO: A análise da evasão escolar sempre requer mais de um ano e a inclusão do 1º semestre.

## Digite o ano inicial e o final
# Anos disponíveis: de 2015 até 2024
anos <- c(2022, 2023)

## Digite o trimestre inicial e final
# P. ex.: todos os trimestres (1, 2, 3, 4); apenas o 3º e 4º trimestres (3, 4) 
# Se a análise for de evasão, recomendada a inclusão apenas do 1º trimestre (1) p/ economizar RAM
trimestres <- c(1, 2, 3, 4)

### Tipo da análise ####
# 'amostra' ou 'censo'
# amostra com 10 mil observações.
tipo_analise <- 'censo' 


#### CARREGAR AMBIENTE ####

### Carregar Bibliotecas ####
## AVISO: Não mexer
source('Codigo Novo pdm\\Script pdm\\bibliotecas.R')

### Local de trabalho ####
## AVISO: Verifique abaixo o caminho do arquivo e altere-o (se ainda não o fez)
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'
dir(local)

### Carregar script de configuração do ambiente ####
## AVISO: Não mexer
source('Codigo Novo pdm\\Script pdm\\0_config.R')

## Verificar dados ####
glimpse(publico_alvo_filtrado)
table(publico_alvo_filtrado$Ano)
table(publico_alvo_filtrado$Trimestre)
