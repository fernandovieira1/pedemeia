### DADOS PNAD -- Estatística Descritiva


######################## 1. BASE EVASÃO ########################
str(base_evasao_filtrada)
names(base_evasao_filtrada)
nrow(base_evasao_filtrada)
ncol(base_evasao_filtrada)

# View(base_evasao_filtrada)

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.1 FAIXA ETÁRIA ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.1.1 Resumo Descritivo da Idade ####
# Resumo estatístico da variável idade
summary(base_evasao_filtrada$V2009)

# Quantidade de valores ausentes (NAs) na variável idade
sum(is.na(base_evasao_filtrada$V2009))

# Verificar os valores únicos de idade
unique(base_evasao_filtrada$V2009)

# Contar a frequência de valores na variável evasao, incluindo NAs
table(base_evasao_filtrada$evasao, useNA = 'ifany')

# Verificar a presença de valores suspeitos (ex.: 0, 9999, negativos)
base_evasao_filtrada %>% 
  filter(V2009 <= 0 | V2009 >= 100) %>%
  count(V2009)

## 1.1.2 Filtragem de Idades Válidas ####
# Filtrar apenas idades entre 14 e 24 anos e garantir que a variável evasao não tenha NAs
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(V2009 >= 14 & V2009 <= 24 & !is.na(evasao))

## 1.1.3 Gráfico Inicial: Distribuição Etária por Evasão** ####
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Criar o gráfico com o período incluído no título
ggplot(base_evasao_filtrada, aes(x = V2009, fill = as.factor(evasao))) +
  geom_histogram(binwidth = 1, position = 'dodge', color = 'black') +
  scale_x_continuous(breaks = seq(min(base_evasao_filtrada$V2009), 
                                  max(base_evasao_filtrada$V2009), 
                                  by = 5)) +
  labs(title = paste0('Distribuição Etária por Evasão Escolar - Acumulado: Período ', inicio, '-', fim),
       x = 'Idade',
       y = 'Frequência',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()

## 1.1.4 Gráfico Filtrado: Idades Válidas (14-24 Anos)** ####
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Filtrar apenas idades entre 14 e 24 anos e remover NAs na evasao
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(V2009 >= 14 & V2009 <= 24 & !is.na(evasao))

# Calcular os percentuais por idade e evasao
base_evasao_percentual <- base_evasao_filtrada_validos %>%
  group_by(V2009, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2009) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Recriar o gráfico com percentuais no topo das barras (ESTE!)
ggplot(base_evasao_percentual, aes(x = as.factor(V2009), y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Inserir percentuais
  scale_x_discrete(breaks = seq(min(as.numeric(base_evasao_percentual$V2009)), 
                                max(as.numeric(base_evasao_percentual$V2009)), 
                                by = 1)) +  # Intervalos de 1 ano
  labs(title = paste0('Distribuição Etária por Evasão Escolar (Idades 14-24) - Acumulado: Período ', inicio, '-', fim),
       x = 'Idade',
       y = 'Frequência',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)


## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.2 COR ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.2.1 Resumo Descritivo da Cor/Raça ####
# Resumo estatístico: Contagem e proporção de evasão por cor/raça
tabela_cor_raca <- base_evasao_filtrada %>%
  group_by(V2010, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2010) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup() %>%
  as.data.frame()  # Converter para data.frame

# Renomear as colunas para evitar problemas com nomes vazios ou inválidos
colnames(tabela_cor_raca) <- c('Cor_Raca', 'Evasao', 'Contagem', 'Proporcao')

# Gerar o título dinâmico com as variáveis 'inicio' e 'fim' sem aspas
titulo_dinamico <- paste0("Proporção de Evasão por Cor/Raça - Período: ", inicio, "-", fim)

# Exibir a tabela com stargazer (em HTML)
tabela_html <- stargazer(
  tabela_cor_raca,
  type = 'html',           # Exportar como HTML
  summary = FALSE,         # Sem resumo
  title = titulo_dinamico, # Título dinâmico com período
  digits = 2               # Número de casas decimais
)

# Converter para um único texto HTML
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer
htmltools::html_print(HTML(html_output))

## 1.2.2 Gráfico Inicial: Proporção de Evasão por Cor/Raça ####
ggplot(base_evasao_filtrada, aes(x = V2010, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Proporção de Evasão por Cor/Raça',
       x = 'Cor/Raça',
       y = 'Proporção (%)',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()


## 1.2.3 Filtragem de Valores Válidos ####
# Filtrar apenas idades válidas e garantir que evasao não tenha NAs
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(V2009 >= 14 & V2009 <= 24 & !is.na(evasao)) %>%
  select(V2010, evasao)  # Selecionar apenas as variáveis relevantes

# Converter a variável V2010 para texto para evitar problemas com fatores
base_evasao_filtrada_validos <- base_evasao_filtrada_validos %>%
  mutate(Cor_Raca = as.character(V2010))

## 1.2.4 Gráfico com Percentuais no Topo** ####
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Adicionar a categoria 'pardos ou pretos'
base_evasao_com_categoria <- base_evasao_filtrada_validos %>%
  mutate(Cor_Raca = as.character(Cor_Raca)) %>% # Converter para texto, se necessário
  bind_rows(
    base_evasao_filtrada_validos %>%
      filter(Cor_Raca %in% c('Parda', 'Preta')) %>%
      mutate(Cor_Raca = 'Pardos ou Pretos') # Nova categoria
  )

# Calcular os percentuais de evasão por cor/raça
base_evasao_percentual <- base_evasao_com_categoria %>%
  group_by(Cor_Raca, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Cor_Raca) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico com os percentuais no topo das barras (ESTE!)
ggplot(base_evasao_percentual, aes(x = Cor_Raca, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = paste0('Evasão por Cor/Raça com Percentuais no Topo - Acumulado: Período ', inicio, '-', fim),
       x = 'Cor/Raça',
       y = 'Frequência',
       fill = 'Evasão (1=Sim)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Melhor visualização

## 1.2.5 Exportação Final da Tabela** ####
# Criar uma tabela consolidada e filtrar valores válidos (remover 'Ignorado' e NAs)
tabela_cor_raca_df <- base_evasao_filtrada_validos %>%
  group_by(Cor_Raca, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Cor_Raca) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

tabela_cor_raca_df_clean <- tabela_cor_raca_df %>%
  filter(Cor_Raca != 'Ignorado' & !is.na(Cor_Raca))

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0("Proporção de Evasão por Cor/Raça (Sem NAs) - Período: ", inicio, "-", fim)

# Exportar com stargazer
stargazer(tabela_cor_raca_df_clean, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

# Filtrar evasão para valores específicos (se necessário)
tabela_cor_raca_df %>%
  filter(Cor_Raca != 'Ignorado' & !is.na(Cor_Raca)) %>%
  filter(evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_cor_raca_df_clean,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## 1.3 SEXO ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
# 1.3.1 Resumo Descritivo de Sexo ####
# Resumo estatístico: Contagem e proporção de evasão por sexo
tabela_sexo <- base_evasao_filtrada %>%
  group_by(V2007, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2007) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Converter o tibble para data.frame
tabela_sexo <- as.data.frame(tabela_sexo)

# Renomear colunas
colnames(tabela_sexo) <- c('Sexo', 'Evasao', 'Contagem', 'Proporcao')

# Exibir a tabela com stargazer
stargazer(tabela_sexo, type = 'text', summary = FALSE,
          title = 'Proporção de Evasão por Sexo',
          digits = 2)

## 1.3.2 Gráfico Inicial: Proporção de Evasão por Sexo ####
ggplot(base_evasao_filtrada, aes(x = V2007, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Proporção de Evasão por Sexo',
       x = 'Sexo',
       y = 'Proporção (%)',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()

## 1.3.3 Gráfico com Percentuais no Topo ####
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Calcular os percentuais de evasão por sexo
base_evasao_percentual_sexo <- base_evasao_filtrada %>%
  group_by(V2007, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2007) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico com os percentuais no topo das barras
ggplot(base_evasao_percentual_sexo, aes(x = V2007, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = paste0('Evasão por Sexo com Percentuais no Topo - Acumulado: Período ', inicio, '-', fim),
       x = 'Sexo',
       y = 'Frequência',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()


## 1.3.4 Gráfico com Percentuais no Topo (Sem NAs)** ####
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Filtrar valores não nulos (NA) em V2007 e evasao
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(!is.na(V2007) & !is.na(evasao))

# Calcular os percentuais de evasão por sexo
base_evasao_percentual_sexo <- base_evasao_filtrada_validos %>%
  group_by(V2007, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2007) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico com os percentuais no topo das barras (ESTE!)
ggplot(base_evasao_percentual_sexo, aes(x = V2007, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = paste0('Evasão por Sexo (Sem NAs) - Acumulado: Período ', inicio, '-', fim),
       x = 'Sexo',
       y = 'Frequência',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()

## 1.3.5 Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Filtrar valores válidos (remover NAs da variável Evasao) (ESTE!)
# Calcular a proporção dentro de cada sexo
tabela_sexo_clean <- tabela_sexo %>%
  filter(!is.na(Evasao)) %>%  # Remover NAs apenas da coluna Evasao
  group_by(Sexo) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%  # Recalcular proporção dentro de cada sexo
  ungroup()

# Verificar a tabela ajustada
print(tabela_sexo_clean)

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0("Proporção de Evasão por Sexo (Recalculada para cada Sexo) - Período: ", inicio, "-", fim)

# Exportar tabela limpa com stargazer
stargazer(tabela_sexo_clean, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

# Filtrar para mostrar apenas evasão = 1
tabela_sexo_clean %>%
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_sexo_clean,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.4 REGIÃO ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.4.1 Resumo Descritivo da Região** ####
# Resumo estatístico: Contagem e proporção de evasão por região
tabela_regiao <- base_evasao_filtrada %>%
  group_by(regiao, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(regiao) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Converter o tibble para data.frame
tabela_regiao <- as.data.frame(tabela_regiao)

# Renomear colunas
colnames(tabela_regiao) <- c('Regiao', 'Evasao', 'Contagem', 'Proporcao')

# Exibir a tabela com stargazer (ESTE!)
stargazer(tabela_regiao, type = 'text', summary = FALSE,
          title = 'Proporção de Evasão por Região',
          digits = 2)

## 1.4.2 Gráfico Inicial: Proporção de Evasão por Região ####
ggplot(base_evasao_filtrada, aes(x = regiao, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Proporção de Evasão por Região',
       x = 'Região',
       y = 'Proporção (%)',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()


## 1.4.3 Gráfico com Percentuais no Topo ####
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Calcular os percentuais de evasão por região
base_evasao_percentual_regiao <- base_evasao_filtrada %>%
  group_by(regiao, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(regiao) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico com os percentuais no topo das barras
ggplot(base_evasao_percentual_regiao, aes(x = regiao, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = paste0('Evasão por Região com Percentuais no Topo - Acumulado: Período ', inicio, '-', fim),
       x = 'Região',
       y = 'Frequência',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()


## 1.4.4 Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar valores não nulos (NA) em regiao e evasao
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(!is.na(regiao) & !is.na(evasao))

# Calcular os percentuais de evasão por região
base_evasao_percentual_regiao <- base_evasao_filtrada_validos %>%
  group_by(regiao, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(regiao) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico com os percentuais no topo das barras (ESTE!)
ggplot(base_evasao_percentual_regiao, aes(x = regiao, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = paste0('Evasão por Região (Sem NAs) - Acumulado: Período ', inicio, '-', fim),
       x = 'Região',
       y = 'Frequência',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()

## 1.4.5 Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Filtrar valores válidos (remover NAs da variável Evasao) (ESTE!)
tabela_regiao_clean <- tabela_regiao %>%
  filter(!is.na(Evasao))  # Remove NAs apenas da coluna Evasao

# Verificar a tabela filtrada
print(tabela_regiao_clean)

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0("Proporção de Evasão por Região (Sem NAs em Evasao) - Período: ", inicio, "-", fim)

# Exportar tabela limpa com stargazer (ESTE!)
stargazer(tabela_regiao_clean, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

# Filtrar para mostrar apenas evasão = 1
tabela_regiao %>%
  filter(!is.na(Evasao)) %>%
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_regiao_clean,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.5 RDPC ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.5.1 Resumo Descritivo do RDPC** ####
# Adicionar o salário mínimo à base, calculado para cada ano
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))  # Adiciona o salário mínimo correspondente ao ano

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo e RDPC
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & !is.na(RDPC) & Salario_Minimo > 0)

# Diagnóstico para verificar valores fora dos limites dos breaks
ajuste <- 1e-4  # Ajuste para evitar problemas de precisão numérica

# Criar categorias de RDPC com `case_when`, baseado nos diagnósticos
tabela_rdpc <- base_evasao_filtrada %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) %>%
  group_by(Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Converter o tibble para data.frame
tabela_rdpc <- as.data.frame(tabela_rdpc)

# Renomear colunas
colnames(tabela_rdpc) <- c('Faixa_RDPC', 'Evasao', 'Contagem', 'Proporcao')

# Exibir a tabela com stargazer (ESTE!)
stargazer(tabela_rdpc, type = 'text', summary = FALSE,
          title = 'Proporção de Evasão por Faixas de RDPC (Ajustadas pelo Salário Mínimo)',
          digits = 2)

## 1.5.2 Gráfico Inicial: Proporção de Evasão por Faixas de RDPC ####
# Gráfico inicial: proporção de evasão por faixas de RDPC ajustadas pelo salário mínimo
ggplot(base_evasao_filtrada %>%
         mutate(
           Faixa_RDPC = case_when(
             RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
             RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
             RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
             RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
             RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
             RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
             TRUE ~ 'Acima de 20 SM'
           )
         ),
       aes(x = Faixa_RDPC, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Proporção de Evasão por Faixas de RDPC (Ajustadas pelo Salário Mínimo)',
    x = 'Faixa de RDPC',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.5.3 Gráfico com Percentuais no Topo ####
# Criar gráfico com os percentuais no topo das barras
ggplot(tabela_rdpc, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = 'Evasão por Faixas de RDPC com Percentuais no Topo (Ajustadas pelo Salário Mínimo)',
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.5.4 Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar valores válidos (sem NAs em evasao e RDPC)
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(!is.na(evasao) & !is.na(RDPC))

# Calcular percentuais com categorias ajustadas
tabela_rdpc_validos <- base_evasao_filtrada_validos %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) %>%
  group_by(Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Evasão por Faixas de RDPC (Sem NAs) - Ajustadas pelo Salário Mínimo - Período: ",
  inicio, "-", fim
)

# Gráfico com percentuais no topo (sem NAs)
ggplot(tabela_rdpc_validos, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = titulo_dinamico,
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 1.5.5 Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Filtrar tabela sem NAs em evasao
tabela_rdpc_clean <- tabela_rdpc %>%
  filter(!is.na(Evasao))  # Remove NAs apenas da coluna evasao

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0("Proporção de Evasão por Faixas de RDPC (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ", inicio, "-", fim)

# Exportar tabela limpa com stargazer (ESTE!)
stargazer(tabela_rdpc_clean, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

# Filtrar para mostrar apenas evasão = 1
tabela_rdpc %>%
  filter(!is.na(Evasao)) %>%
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_clean,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.6 RDPC POR REGIÃO ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.6.1 Resumo Descritivo do RDPC por Região** ####
# Adicionar o salário mínimo à base, calculado para cada ano
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))  # Adiciona o salário mínimo correspondente ao ano

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo, RDPC e Região
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & !is.na(RDPC) & Salario_Minimo > 0 & !is.na(regiao))

# Diagnóstico para verificar valores fora dos limites dos breaks
ajuste <- 1e-4  # Ajuste para evitar problemas de precisão numérica

# Criar categorias de RDPC por região
tabela_rdpc_regiao <- base_evasao_filtrada %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) %>%
  group_by(regiao, Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(regiao, Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Converter o tibble para data.frame
tabela_rdpc_regiao <- as.data.frame(tabela_rdpc_regiao)

# Renomear colunas
colnames(tabela_rdpc_regiao) <- c('Regiao', 'Faixa_RDPC', 'Evasao', 'Contagem', 'Proporcao')

# Exibir a tabela com stargazer (ESTE!)
stargazer(tabela_rdpc_regiao, type = 'text', summary = FALSE,
          title = 'Proporção de Evasão por Faixas de RDPC por Região (Ajustadas pelo Salário Mínimo)',
          digits = 2)

## 1.6.2 Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Região ####
# Gráfico inicial: proporção de evasão por faixas de RDPC por região
ggplot(base_evasao_filtrada %>%
         mutate(
           Faixa_RDPC = case_when(
             RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
             RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
             RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
             RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
             RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
             RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
             TRUE ~ 'Acima de 20 SM'
           )
         ),
       aes(x = Faixa_RDPC, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  facet_wrap(~regiao) +  # Facetar por região
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Proporção de Evasão por Faixas de RDPC por Região',
    x = 'Faixa de RDPC',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.6.3 Gráfico com Percentuais no Topo ####
# Criar gráfico com os percentuais no topo das barras, segmentado por região
ggplot(tabela_rdpc_regiao, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~Regiao) +  # Facetar por região
  labs(
    title = 'Evasão por Faixas de RDPC com Percentuais no Topo por Região',
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.6.4 Gráfico com Percentuais no Topo (Sem NAs)** ####
# Gráfico com percentuais no topo (sem NAs) por região, com texto rotacionado em 90 graus
# Criar tabela filtrada sem valores NA em evasao
tabela_rdpc_regiao_validos <- tabela_rdpc_regiao %>%
  filter(!is.na(Evasao))  # Remover apenas os valores NA em evasao

# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Evasão por Faixas de RDPC (Sem NAs) por Região - Período: ",
  inicio, "-", fim
)

ggplot(tabela_rdpc_regiao_validos, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) + # (ESTE!)
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 1.9),
    angle = 45,  # Rotação do texto
    vjust = 0,  # Ajuste vertical para centralizar
    hjust = 0.3,    # Ajuste horizontal para alinhar
    size = 3
  ) +
  facet_wrap(~Regiao) +  # Facetar por região
  labs(
    title = titulo_dinamico,
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 1.6.5 Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Filtrar tabela sem NAs em evasao e regiao
tabela_rdpc_regiao_clean <- tabela_rdpc_regiao %>%
  filter(!is.na(Evasao) & !is.na(Regiao))  # Remove NAs

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0("Proporção de Evasão por Faixas de RDPC por Região (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ", inicio, "-", fim)

# Exportar tabela limpa com stargazer (ESTE!)
stargazer(tabela_rdpc_regiao_clean, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

# Filtrar para mostrar apenas evasão = 1
tabela_rdpc_regiao %>%
  filter(!is.na(Evasao) & !is.na(Regiao)) %>%
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_regiao_clean,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.7 RDPC POR COR ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.7.1 Resumo Descritivo do RDPC por Cor** ####
# Adicionar o salário mínimo à base, calculado para cada ano
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))  # Adiciona o salário mínimo correspondente ao ano

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo, RDPC e V2010 (cor)
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & !is.na(RDPC) & Salario_Minimo > 0 & !is.na(V2010))

# Diagnóstico para verificar valores fora dos limites dos breaks
ajuste <- 1e-4  # Ajuste para evitar problemas de precisão numérica

# Criar categorias de RDPC por cor
tabela_rdpc_cor <- base_evasao_filtrada %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) %>%
  group_by(V2010, Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2010, Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Converter o tibble para data.frame
tabela_rdpc_cor <- as.data.frame(tabela_rdpc_cor)

# Renomear colunas
colnames(tabela_rdpc_cor) <- c('Cor', 'Faixa_RDPC', 'Evasao', 'Contagem', 'Proporcao')

# Exibir a tabela com stargazer (ESTE!)
stargazer(tabela_rdpc_cor, type = 'text', summary = FALSE,
          title = 'Proporção de Evasão por Faixas de RDPC por Cor (Ajustadas pelo Salário Mínimo)',
          digits = 2)

## 1.7.2 Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Cor ####
# Gráfico inicial: proporção de evasão por faixas de RDPC por cor
ggplot(base_evasao_filtrada %>%
         mutate(
           Faixa_RDPC = case_when(
             RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
             RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
             RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
             RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
             RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
             RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
             TRUE ~ 'Acima de 20 SM'
           )
         ),
       aes(x = Faixa_RDPC, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  facet_wrap(~V2010) +  # Facetar por cor
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Proporção de Evasão por Faixas de RDPC por Cor',
    x = 'Faixa de RDPC',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.7.3 Gráfico com Percentuais no Topo ####
# Criar gráfico com os percentuais no topo das barras, segmentado por cor
ggplot(tabela_rdpc_cor, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~Cor) +  # Facetar por cor
  labs(
    title = 'Evasão por Faixas de RDPC com Percentuais no Topo por Cor',
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.7.4 Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar apenas dados válidos (sem NAs em Evasao e V2010)
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(!is.na(evasao) & !is.na(V2010) & !is.na(RDPC))

# Criar categorias de RDPC por cor, filtrando valores válidos
tabela_rdpc_cor_validos <- base_evasao_filtrada_validos %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) %>%
  group_by(V2010, Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2010, Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Evasão por Faixas de RDPC (Sem NAs) por Cor - Período: ",
  inicio, "-", fim
)

# Gráfico com percentuais no topo (sem NAs)
ggplot(tabela_rdpc_cor_validos, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    angle = 90,  # Rotação do texto
    vjust = 0.5,
    hjust = 1,
    size = 3.5
  ) +
  facet_wrap(~V2010) +  # Facetar por cor
  labs(
    title = titulo_dinamico,  # Título dinâmico
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.7.5 Exportação Final da Tabela (Sem NAs em Evasao e Cor)** ####
# Filtrar tabela sem NAs em evasao e V2010
tabela_rdpc_cor_clean <- tabela_rdpc_cor %>%
  filter(!is.na(Evasao) & !is.na(Cor))  # Remove NAs

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0("Proporção de Evasão por Faixas de RDPC por Cor (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ", inicio, "-", fim)

# Exportar tabela limpa com stargazer (ESTE!)
stargazer(tabela_rdpc_cor_clean, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

# Filtrar para mostrar apenas evasão = 1
tabela_rdpc_cor %>%
  filter(!is.na(Evasao) & !is.na(Cor)) %>%
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_cor_clean,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.8 RDPC POR SEXO** ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.8.1 Resumo Descritivo do RDPC por Sexo** ####
# Adicionar o salário mínimo à base, calculado para cada ano
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))  # Adiciona o salário mínimo correspondente ao ano

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo, RDPC e V2007 (sexo)
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & !is.na(RDPC) & Salario_Minimo > 0 & !is.na(V2007))

# Criar categorias de RDPC por sexo
tabela_rdpc_sexo <- base_evasao_filtrada %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) %>%
  group_by(V2007, Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2007, Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Converter o tibble para data.frame
tabela_rdpc_sexo <- as.data.frame(tabela_rdpc_sexo)

# Renomear colunas
colnames(tabela_rdpc_sexo) <- c('Sexo', 'Faixa_RDPC', 'Evasao', 'Contagem', 'Proporcao')

# Exibir a tabela com stargazer (ESTE!)
stargazer(tabela_rdpc_sexo, type = 'text', summary = FALSE,
          title = 'Proporção de Evasão por Faixas de RDPC por Sexo (Ajustadas pelo Salário Mínimo)',
          digits = 2)

## 1.8.2 Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Sexo ####
ggplot(base_evasao_filtrada %>%
         mutate(
           Faixa_RDPC = case_when(
             RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
             RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
             RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
             RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
             RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
             RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
             TRUE ~ 'Acima de 20 SM'
           )
         ),
       aes(x = Faixa_RDPC, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  facet_wrap(~V2007) +  # Facetar por sexo
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Proporção de Evasão por Faixas de RDPC por Sexo',
    x = 'Faixa de RDPC',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.8.3 Gráfico com Percentuais no Topo ####
ggplot(tabela_rdpc_sexo, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~Sexo) +  # Facetar por sexo
  labs(
    title = 'Evasão por Faixas de RDPC com Percentuais no Topo por Sexo',
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.8.4 Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar dados válidos
tabela_rdpc_sexo_validos <- tabela_rdpc_sexo %>%
  filter(!is.na(Evasao))

# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Evasão por Faixas de RDPC (Sem NAs) por Sexo - Período: ",
  inicio, "-", fim
)

# Criar gráfico com percentuais no topo
ggplot(tabela_rdpc_sexo_validos, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 1.9),
    angle = 90,
    vjust = 0.5,
    hjust = 1,
    size = 3
  ) +
  facet_wrap(~Sexo) +  # Facetar por sexo
  labs(
    title = titulo_dinamico,  # Título dinâmico
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 1.8.5 Exportação Final da Tabela (Sem NAs em Evasao e Sexo)** ####
tabela_rdpc_sexo_clean <- tabela_rdpc_sexo %>%
  filter(!is.na(Evasao) & !is.na(Sexo))  # Remove NAs de Evasao e Sexo

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0("Proporção de Evasão por Faixas de RDPC por Sexo (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ", inicio, "-", fim)

# Exportar tabela limpa com stargazer (ESTE!)
stargazer(tabela_rdpc_sexo_clean, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

# Filtrar para mostrar apenas evasão = 1
tabela_rdpc_sexo_clean %>%
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_sexo_clean,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.9 RDPC POR ENSINO MÉDIO ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.9.1 Resumo Descritivo do RDPC por Ensino Médio** ####
# Adicionar o salário mínimo à base, calculado para cada ano
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))  # Adiciona o salário mínimo correspondente ao ano

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo, RDPC e Ensino Médio
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & !is.na(RDPC) & Salario_Minimo > 0 & !is.na(V3002A))

# Criar categorias de RDPC por ensino médio
tabela_rdpc_ensino_medio <- base_evasao_filtrada %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) %>%
  group_by(V3002A, Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V3002A, Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Converter o tibble para data.frame
tabela_rdpc_ensino_medio <- as.data.frame(tabela_rdpc_ensino_medio)

# Renomear colunas
colnames(tabela_rdpc_ensino_medio) <- c('Ensino_Medio', 'Faixa_RDPC', 'Evasao', 'Contagem', 'Proporcao')

# Exibir a tabela com stargazer (ESTE!)
stargazer(tabela_rdpc_ensino_medio, type = 'text', summary = FALSE,
          title = 'Proporção de Evasão por Faixas de RDPC por Ensino Médio (Ajustadas pelo Salário Mínimo)',
          digits = 2)

tabela_rdpc_ensino_medio %>% 
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_ensino_medio,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = 'Proporção de Evasão por Faixas de RDPC por Ensino Médio (Ajustadas pelo Salário Mínimo)',  # Título
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))


## 1.9.2 Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Ensino Médio ####
ggplot(base_evasao_filtrada %>%
         mutate(
           Faixa_RDPC = case_when(
             RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
             RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
             RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
             RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
             RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
             RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
             TRUE ~ 'Acima de 20 SM'
           )
         ),
       aes(x = Faixa_RDPC, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  facet_wrap(~V3002A) +  # Facetar por ensino médio
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = 'Proporção de Evasão por Faixas de RDPC por Ensino Médio',
    x = 'Faixa de RDPC',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.9.3 Gráfico com Percentuais no Topo  ####
ggplot(tabela_rdpc_ensino_medio, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~Ensino_Medio) +  # Facetar por ensino médio
  labs(
    title = 'Evasão por Faixas de RDPC com Percentuais no Topo por Ensino Médio',
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.9.4 Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar dados válidos
tabela_rdpc_ensino_medio_validos <- tabela_rdpc_ensino_medio %>%
  filter(!is.na(Evasao))

# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Evasão por Faixas de RDPC (Sem NAs) por Ensino Médio - Período: ",
  inicio, "-", fim
)

# Criar gráfico com percentuais no topo
ggplot(tabela_rdpc_ensino_medio_validos, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    angle = 0,
    vjust = 2,
    hjust = 1,
    size = 3
  ) +
  facet_wrap(~Ensino_Medio) +  # Facetar por ensino médio
  labs(
    title = titulo_dinamico,  # Título dinâmico
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 1.9.5 Exportação Final da Tabela (Sem NAs em Evasao e Ensino Médio)** ####
tabela_rdpc_ensino_medio_clean <- tabela_rdpc_ensino_medio %>%
  filter(!is.na(Evasao) & !is.na(Ensino_Medio))  # Remove NAs de Evasao e Ensino Médio

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0("Proporção de Evasão por Faixas de RDPC por Ensino Médio (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ", inicio, "-", fim)

# Exportar tabela limpa com stargazer (ESTE!)
stargazer(tabela_rdpc_ensino_medio_clean, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

# Filtrar para mostrar apenas evasão = 1
tabela_rdpc_ensino_medio %>%
  filter(!is.na(Evasao) & !is.na(Ensino_Medio)) %>%
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_ensino_medio_clean,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.10 RDPC POR EVASÃO ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.10.1 Resumo Descritivo do RDPC por Evasão** ####
# Adicionar o salário mínimo à base, calculado para cada ano
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))  # Adiciona o salário mínimo correspondente ao ano

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo, RDPC e evasao
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & !is.na(RDPC) & Salario_Minimo > 0 & !is.na(evasao))

# Criar categorias de RDPC por evasão
tabela_rdpc_evasao <- base_evasao_filtrada %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) %>%
  group_by(evasao, Faixa_RDPC) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Converter o tibble para data.frame
tabela_rdpc_evasao <- as.data.frame(tabela_rdpc_evasao)

# Renomear colunas
colnames(tabela_rdpc_evasao) <- c('Evasao', 'Faixa_RDPC', 'Contagem', 'Proporcao')

# Gerar título dinâmico com as variáveis 'inicio' e 'fim'
titulo_dinamico <- paste0("Proporção de Evasão por Faixas de RDPC (Ajustadas pelo Salário Mínimo) - Período: ", inicio, "-", fim)

# Exibir a tabela com stargazer (ESTE!)
stargazer(tabela_rdpc_evasao, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

tabela_rdpc_evasao %>% 
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_evasao,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

## 1.10.2 Gráfico Inicial: Proporção de Evasão por Faixas de RDPC** ####
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Proporção de Evasão por Faixas de RDPC - Período: ",
  inicio, "-", fim
)

# Gráfico inicial: proporção de evasão por faixas de RDPC
ggplot(base_evasao_filtrada %>%
         mutate(
           Faixa_RDPC = case_when(
             RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
             RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
             RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
             RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
             RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
             RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
             TRUE ~ 'Acima de 20 SM'
           )
         ),
       aes(x = Faixa_RDPC, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) + # Intervalos de 10%
  labs(
    title = titulo_dinamico,  # Título dinâmico
    x = 'Faixa de RDPC',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.10.3 Gráfico com Percentuais no Topo** ####
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Evasão por Faixas de RDPC com Percentuais no Topo - Período: ",
  inicio, "-", fim
)

# Criar gráfico com os percentuais no topo das barras, segmentado por evasão
ggplot(tabela_rdpc_evasao, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = titulo_dinamico,  # Título dinâmico
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.10.4 Gráfico com Percentuais no Topo (Sem NAs) ####
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Evasão por Faixas de RDPC (Sem NAs) - Período: ",
  inicio, "-", fim
)

# Filtrar tabela sem NAs em evasao
tabela_rdpc_evasao_validos <- tabela_rdpc_evasao %>%
  filter(!is.na(Evasao))

# Gráfico com percentuais no topo (sem NAs), com texto rotacionado em 90 graus
ggplot(tabela_rdpc_evasao_validos, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    angle = 90,  # Rotação do texto
    vjust = 0.5,
    hjust = 1,
    size = 3.5
  ) +
  labs(
    title = titulo_dinamico,  # Título dinâmico
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 1.10.5 Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Gerar título dinâmico com as variáveis 'inicio' e 'fim'
titulo_dinamico <- paste0("Proporção de Evasão por Faixas de RDPC (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ", inicio, "-", fim)

# Exportar tabela limpa com stargazer (ESTE!)
stargazer(tabela_rdpc_evasao_validos, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_evasao_validos,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.11 RDPC POR EVASÃO E ENSINO MÉDIO** ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.11.1 Resumo Descritivo do RDPC por Evasão e Ensino Médio** ####
# Adicionar o salário mínimo à base, calculado para cada ano
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))  # Adiciona o salário mínimo correspondente ao ano

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo, RDPC, evasao e ensino médio
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & 
           !is.na(RDPC) & Salario_Minimo > 0 & 
           !is.na(evasao) & !is.na(V3002A))

# Criar categorias de RDPC por evasão e ensino médio
tabela_rdpc_evasao_ensino <- base_evasao_filtrada %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) %>%
  group_by(V3002A, evasao, Faixa_RDPC) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V3002A, Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Converter o tibble para data.frame
tabela_rdpc_evasao_ensino <- as.data.frame(tabela_rdpc_evasao_ensino)

# Renomear colunas
colnames(tabela_rdpc_evasao_ensino) <- c('Ensino_Medio', 'Evasao', 'Faixa_RDPC', 'Contagem', 'Proporcao')

# Gerar título dinâmico com período
titulo_dinamico <- paste0("Proporção de Evasão por Faixas de RDPC e Ensino Médio (Ajustadas pelo Salário Mínimo) - Período: ", inicio, "-", fim)

# Exportar tabela com stargazer (ESTE!)
stargazer(tabela_rdpc_evasao_ensino, type = 'text', summary = FALSE,
          title = titulo_dinamico,
          digits = 2)

tabela_rdpc_evasao_ensino %>% 
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_evasao_ensino,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))


## 1.11.2 Gráfico Inicial: Proporção de Evasão por Faixas de RDPC e Ensino Médio ####
# Gráfico inicial: proporção de evasão por faixas de RDPC segmentado por ensino médio
# Não faz sentido

## 1.11.3 Gráfico com Percentuais no Topo ####
# Não faz sentido

## 1.11.4 Gráfico com Percentuais no Topo (Sem NAs)** ####
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Evasão por Faixas de RDPC e Ensino Médio (Sem NAs) - Período: ",
  inicio, "-", fim
)

# Filtrar tabela sem NAs em evasao e ensino médio
tabela_rdpc_evasao_ensino_validos <- tabela_rdpc_evasao_ensino %>%
  filter(!is.na(Evasao) & !is.na(Ensino_Medio))

# Gráfico com percentuais no topo (sem NAs)
ggplot(tabela_rdpc_evasao_ensino_validos, 
       aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0(Proporcao, '%')),
    position = position_dodge(width = 0.9),
    vjust = -0.5,  # Coloca o texto acima das barras
    size = 3.5
  ) +
  facet_wrap(~Ensino_Medio) +  # Facetar apenas por Ensino_Medio
  labs(
    title = titulo_dinamico,  # Título dinâmico
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


## 1.11.5 Exportação Final da Tabela (Sem NAs em Evasao e Ensino Médio)** #### 
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Proporção de Evasão por Faixas de RDPC e Ensino Médio (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ",
  inicio, "-", fim
)

# Exportar tabela limpa com stargazer (ESTE!)
stargazer(
  tabela_rdpc_evasao_ensino_validos, 
  type = 'text', 
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2
)

tabela_rdpc_evasao_ensino %>%
  filter(!is.na(Evasao) & !is.na(Ensino_Medio)) %>%
  filter(Evasao == 1)

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_evasao_ensino_validos,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.12 RESUMO EVASÃO ####

#### ////// TEMPO: DADOS EMPILHADOS ////// ####
## 1.12.1 Resumo Descritivo da Evasão** ####
# Resumo descritivo da evasão
tabela_evasao <- base_evasao_filtrada %>%
  group_by(evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2))
tabela_evasao

## 1.12.2 Gráfico Inicial: Proporção de Evasão** #### 
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Proporção de Evasão - Período: ",
  inicio, "-", fim
)

# Gráfico inicial: proporção de evasão
ggplot(tabela_evasao, aes(x = '', y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', color = 'black') +
  geom_text(aes(label = paste0(Proporcao, '%')), vjust = -0.5, size = 5) +
  labs(
    title = titulo_dinamico,  # Título dinâmico
    x = '',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()


## 1.12.3 Exportação Final da Tabela (Sem NAs em Evasao)** #### 
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  "Proporção de Evasão (Sem NAs) - Período: ",
  inicio, "-", fim
)

# Exportar tabela limpa com stargazer
stargazer(tabela_evasao, type = 'text', summary = FALSE,
          title = titulo_dinamico,  # Título dinâmico
          digits = 2)

# Estes valores batem com: 
prop.table(round(table(base_evasao_filtrada$evasao)))

# Gerar a tabela em formato HTML com stargazer
tabela_html <- stargazer(
  tabela_evasao,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = titulo_dinamico,  # Título dinâmico
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = "\n")

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// TEMPO: DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####
## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
### 1.13 MOTIVOS DE EVASÃO ####
# - Análise preliminar e acessória de motivos de evasão, complementando a análise de evasão
# utilizando dados empilhados e longitudinais, a fim de identificar possíveis fatores associados
# à evasão escolar que poderão ser testados nos modelos probit, logit e heckit.

# Filtrar apenas indivíduos válidos para a análise
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(V2009 >= 14 & V2009 <= 24 & !is.na(evasao))

## 1.13.1 Motivos Financeiros ####
# Comparar evasão com base na renda per capita, rendimento habitual e transferências sociais
tabela_motivos_financeiros <- base_evasao_filtrada_validos %>%
  group_by(evasao) %>%
  summarise(
    `Média RDPC` = mean(VD2003, na.rm = TRUE),
    `Média Rendimento Habitual` = mean(VD4017, na.rm = TRUE),
    `Média Transferências Sociais` = mean(VD4047, na.rm = TRUE)
  )

# Exibir tabela formatada
stargazer(
  tabela_motivos_financeiros,
  type = "text",
  summary = FALSE,
  title = "Motivos Financeiros: Evasão e Indicadores Econômicos",
  digits = 2
)

# Gráfico de renda per capita por evasão
ggplot(base_evasao_filtrada_validos, aes(x = as.factor(evasao), y = VD2003)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Renda Domiciliar Per Capita por Evasão",
    x = "Evasão (1=Sim)",
    y = "Renda Domiciliar Per Capita (VD2003)"
  ) +
  theme_minimal()

## 1.13.2 Falta de Infraestrutura ####
# Análise de evasão por localização (urbana/rural)
tabela_infraestrutura <- base_evasao_filtrada_validos %>%
  group_by(V1014, evasao) %>%
  summarise(Contagem = n(), .groups = "drop") %>%
  group_by(V1014) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2))

# Exibir tabela formatada
stargazer(
  tabela_infraestrutura,
  type = "text",
  summary = FALSE,
  title = "Evasão por Localização (Urbano/Rural)",
  digits = 2
)

# Gráfico de evasão por localização
ggplot(tabela_infraestrutura, aes(x = V1014, y = Proporcao, fill = as.factor(evasao))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Evasão por Localização (Urbano/Rural)",
    x = "Localização (1=Urbano, 2=Rural)",
    y = "Proporção (%)",
    fill = "Evasão (1=Sim)"
  ) +
  theme_minimal()

## 1.13.3 Necessidade de Trabalhar ####
# Jovens ocupados com carga de trabalho e evasão
base_evasao_trabalho <- base_evasao_filtrada_validos %>%
  mutate(
    Trabalha = ifelse(!is.na(V4001) & V4001 == 1, "Sim", "Não")
  )

tabela_trabalho_evasao <- base_evasao_trabalho %>%
  group_by(Trabalha, evasao) %>%
  summarise(Contagem = n(), .groups = "drop") %>%
  group_by(Trabalha) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2))

# Exibir tabela formatada
stargazer(
  tabela_trabalho_evasao,
  type = "text",
  summary = FALSE,
  title = "Evasão e Necessidade de Trabalhar",
  digits = 2
)

# Gráfico de evasão por carga de trabalho
ggplot(tabela_trabalho_evasao, aes(x = Trabalha, y = Proporcao, fill = as.factor(evasao))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Evasão por Necessidade de Trabalhar",
    x = "Trabalha",
    y = "Proporção (%)",
    fill = "Evasão (1=Sim)"
  ) +
  theme_minimal()

## 1.13.4 Fatores Domésticos e Culturais ####
# Análise por sexo e arranjo familiar
tabela_domestico_cultural <- base_evasao_filtrada_validos %>%
  group_by(VD2002, V2007, evasao) %>%
  summarise(Contagem = n(), .groups = "drop") %>%
  group_by(VD2002, V2007) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2))

# Exibir tabela formatada
stargazer(
  tabela_domestico_cultural,
  type = "text",
  summary = FALSE,
  title = "Evasão por Arranjo Familiar e Sexo",
  digits = 2
)

# Gráfico de evasão por sexo e arranjo familiar
ggplot(tabela_domestico_cultural, aes(x = as.factor(VD2002), y = Proporcao, fill = as.factor(evasao))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~V2007, labeller = labeller(V2007 = c("1" = "Masculino", "2" = "Feminino"))) +
  labs(
    title = "Evasão por Arranjo Familiar e Sexo",
    x = "Arranjo Familiar (VD2002)",
    y = "Proporção (%)",
    fill = "Evasão (1=Sim)"
  ) +
  theme_minimal()

## 1.13.5 Transferências Sociais ####
# Cruzar valores de transferências sociais com evasão
tabela_transferencias <- base_evasao_filtrada_validos %>%
  group_by(evasao) %>%
  summarise(
    `Média Transferências` = mean(VD4047, na.rm = TRUE)
  )

# Exibir tabela formatada
stargazer(
  tabela_transferencias,
  type = "text",
  summary = FALSE,
  title = "Transferências Sociais e Evasão",
  digits = 2
)

# Gráfico de valores médios de transferências sociais
ggplot(base_evasao_filtrada_validos, aes(x = as.factor(evasao), y = VD4047)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Transferências Sociais por Evasão",
    x = "Evasão (1=Sim)",
    y = "Transferências Sociais (VD4047)"
  ) +
  theme_minimal()




