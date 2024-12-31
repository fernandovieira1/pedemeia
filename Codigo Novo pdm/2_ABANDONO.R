### DADOS PNAD -- Estatística Descritiva
# - Análise descritiva dos dados de evasão escolar.
# - Dados empilhados e longitudinais, 
# - Análise prelimar que pode indicar variáveis a serem testadas 
# nos modelos probit, logit e heckit.

# Limpar o ambiente
gc(); cat('\014')

## Carregar script Evasão Escolar ####
# AVISO: Não mexer
source('Codigo Novo pdm\\Script pdm\\2_abandono.R')
glimpse(base_abandono_filtrada)

######################## 2. BASE ABANDONO ########################
cat('\014')

str(base_abandono_filtrada)
names(base_abandono_filtrada)
nrow(base_abandono_filtrada)
ncol(base_abandono_filtrada)
# summary(base_abandono_filtrada)

# View(base_abandono_filtrada)

# Qtde domicílios (ABANDONO)
base_abandono_filtrada %>% 
  group_by(ID_DOMICILIO) %>%
  count()

# Qtde indivíduos (ABANDONO)
base_abandono_filtrada %>% 
  group_by(id_individuo) %>%
  count()

# Contar a frequência de valores na variável abandono, incluindo NAs
table(base_abandono_filtrada$abandono, useNA = 'ifany')
prop.table(table(base_abandono_filtrada$abandono, useNA = 'ifany'))

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.1 FAIXA ETÁRIA ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.1.1A Resumo Descritivo da Idade ####
# Resumo estatístico da variável idade
summary(base_abandono_filtrada$V2009)

# Quantidade de valores ausentes (NAs) na variável idade
sum(is.na(base_abandono_filtrada$V2009))

# Verificar os valores únicos de idade
unique(base_abandono_filtrada$V2009)

# Verificar a presença de valores suspeitos (ex.: 0, 9999, negativos)
base_abandono_filtrada %>% 
  filter(V2009 <= 0 | V2009 >= 100) %>%
  count(V2009) %>% head()

## 1.1.2A Filtragem de Idades Válidas ####
# Filtrar apenas idades entre 14 e 24 anos e garantir que a variável abandono não tenha NAs
base_abandono_pdm <- base_abandono_filtrada %>%
  filter(V2009 >= 14 & V2009 <= 24 & !is.na(abandono))

summary(base_abandono_pdm)
nrow(base_abandono_pdm)

## 1.1.3A Gráfico Inicial: Distribuição Etária por Evasão** ####
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Criar o gráfico com o período incluído no título
ggplot(base_abandono_filtrada, aes(x = V2009, fill = as.factor(abandono))) +
  geom_histogram(binwidth = 1, position = 'dodge', color = 'black') +
  scale_x_continuous(breaks = seq(min(base_abandono_filtrada$V2009), 
                                  max(base_abandono_filtrada$V2009), 
                                  by = 5)) +
  labs(title = paste0('Distribuição Etária por Abandono Escolar - Acumulado: Período ', inicio, '-', fim),
       x = 'Idade',
       y = 'Frequência',
       fill = 'Abandono (1=Sim)') +
  theme_minimal()





















### Criar Variáveis Necessárias ####
# Adaptar a lógica de abandono escolar com base em '2_abandono.R'
base_abandono_filtrada <- base_abandono_filtrada %>%
  mutate(
    RDPC_menor_meio_sm = if_else(RDPC < 0.5 * salario_minimo, 1, 0, missing = 0),
    abandono = if_else(!is.na(V3008) & V3008 == 'Sim' & V3002 == 'Não', 1, 0, missing = 0)
  )

### Estatística Descritiva ####
head(base_abandono_filtrada)




# Distribuição por Região e Ano
table(base_abandono_filtrada$regiao)
prop.table(table(base_abandono_filtrada$regiao))

