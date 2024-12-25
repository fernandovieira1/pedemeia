### DADOS PNAD -- Estatística Descritiva


######################## 1. BASE EVASÃO ########################
str(base_evasao_filtrada)
names(base_evasao_filtrada)
nrow(base_evasao_filtrada)
ncol(base_evasao_filtrada)
summary(base_evasao_filtrada) # ver coluna 'transferencias_sociais'

# View(base_evasao_filtrada)

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.1 FAIXA ETÁRIA ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.1.1A Resumo Descritivo da Idade ####
# Resumo estatístico da variável idade
summary(base_evasao_filtrada$V2009)

# Quantidade de valores ausentes (NAs) na variável idade
sum(is.na(base_evasao_filtrada$V2009))

# Verificar os valores únicos de idade
unique(base_evasao_filtrada$V2009)

# Contar a frequência de valores na variável evasao, incluindo NAs
table(base_evasao_filtrada$evasao, useNA = 'ifany')
prop.table(table(base_evasao_filtrada$evasao, useNA = 'ifany'))

# Verificar a presença de valores suspeitos (ex.: 0, 9999, negativos)
base_evasao_filtrada %>% 
  filter(V2009 <= 0 | V2009 >= 100) %>%
  count(V2009) %>% head()

## 1.1.2A Filtragem de Idades Válidas ####
# Filtrar apenas idades entre 14 e 24 anos e garantir que a variável evasao não tenha NAs
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(V2009 >= 14 & V2009 <= 24 & !is.na(evasao))

summary(base_evasao_filtrada_validos)
nrow(base_evasao_filtrada_validos)

## 1.1.3A Gráfico Inicial: Distribuição Etária por Evasão** ####
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

## 1.1.4A Gráfico Filtrado: Idades Válidas (14-24 Anos)** ####
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

# Recriar o gráfico com percentuais no topo das barras
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

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 4)

## 1.1.1B Resumo Descritivo da Idade ####
resumo_idade_ano <- base_evasao_filtrada %>%
  group_by(Ano) %>%
  summarise(
    Min = min(V2009, na.rm = TRUE),
    Máximo = max(V2009, na.rm = TRUE),
    Média = round(mean(V2009, na.rm = TRUE), 2),
    Mediana = median(V2009, na.rm = TRUE),
    `Desvio Padrão` = round(sd(V2009, na.rm = TRUE), 2),
    `Valores Ausentes` = sum(is.na(V2009))
  )

# Gerar tabela HTML com stargazer
tabela_html <- stargazer(
  resumo_idade_ano,
  type = 'html',
  summary = FALSE,
  title = 'Resumo Estatístico da Idade por Ano'
)

# Renderizar no Viewer
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))

## 1.1.2B Filtragem de Idades Válidas ####
# Filtrar idades válidas entre 14 e 24 anos e remover NAs na variável evasao
base_evasao_filtrada_validos_ano <- base_evasao_filtrada %>%
  filter(V2009 >= 14 & V2009 <= 24 & !is.na(evasao))

# Contagem de idades válidas por ano e por idade
contagem_validos_ano_idade <- base_evasao_filtrada_validos_ano %>%
  group_by(Ano, V2009) %>%
  summarise(Contagem = n(), .groups = 'drop')

# Adicionar linha de total por ano
contagem_validos_totais <- base_evasao_filtrada_validos_ano %>%
  group_by(Ano) %>%
  summarise(V2009 = 'Total', Contagem = n(), .groups = 'drop')

# Converter V2009 para character em ambas as tabelas para garantir compatibilidade
contagem_validos_ano_idade <- contagem_validos_ano_idade %>%
  mutate(V2009 = as.character(V2009))

# Combinar tabelas segmentadas com totais
tabela_completa <- bind_rows(contagem_validos_ano_idade, contagem_validos_totais)

# Ordenar para garantir a visualização correta
tabela_completa <- tabela_completa %>%
  arrange(Ano, V2009)

# Exibir a tabela com stargazer (em HTML)
tabela_html <- stargazer(
  tabela_completa,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = 'Contagem de Idades Válidas (14-24 Anos) Segmentada por Ano',
  digits = 0,               # Número de casas decimais
  rownames = FALSE          # Sem nomes de linha
)

# Converter para um único texto HTML
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer
htmltools::html_print(HTML(html_output))

# Criar o gráfico com os totais acima das barras
ggplot(contagem_validos_ano_idade, aes(x = as.factor(V2009), y = Contagem, fill = as.factor(Ano))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), color = 'black') +
  geom_text(aes(label = Contagem), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3) + # Adiciona os totais acima das barras
  labs(
    title = 'Distribuição de Idades Válidas (14-24 Anos) Segmentada por Ano',
    x = 'Idade',
    y = 'Contagem',
    fill = 'Ano'
  ) +
  scale_fill_brewer(palette = 'Set2') +
  theme_minimal()

## 1.1.3B Gráfico Inicial: Distribuição Etária por Evasão** ####
# Gráfico para cada ano
ggplot(base_evasao_filtrada, aes(x = V2009, fill = as.factor(evasao))) +
  geom_histogram(binwidth = 1, position = 'dodge', color = 'black') +
  scale_x_continuous(breaks = seq(min(base_evasao_filtrada$V2009), max(base_evasao_filtrada$V2009), by = 5)) +
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Distribuição Etária por Evasão Escolar - Separado por Ano',
    x = 'Idade',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.1.4B Gráfico Filtrado: Idades Válidas (14-24 Anos)** ####
# Calcular percentuais por idade, evasao e ano
base_evasao_percentual_ano <- base_evasao_filtrada_validos_ano %>%
  group_by(Ano, V2009, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, V2009) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico por ano com percentuais
ggplot(base_evasao_percentual_ano, aes(x = as.factor(V2009), y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Inserir percentuais
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Distribuição Etária por Evasão Escolar (Idades 14-24) - Separado por Ano',
    x = 'Idade',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.2 COR ####

#### ////// (A) DADOS EMPILHADOS ////// ####

## 1.2.1A Resumo Descritivo da Cor/Raça ####
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
titulo_dinamico <- paste0('Proporção de Evasão por Cor/Raça - Período: ', inicio, '-', fim)

# Exibir a tabela com stargazer (em HTML)
tabela_html <- stargazer(
  tabela_cor_raca,
  type = 'html',           # Exportar como HTML
  summary = FALSE,         # Sem resumo
  title = titulo_dinamico, # Título dinâmico com período
  digits = 2               # Número de casas decimais
)

# Converter para um único texto HTML
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer
htmltools::html_print(HTML(html_output))

## 1.2.2A Gráfico Inicial: Proporção de Evasão por Cor/Raça ####
ggplot(base_evasao_filtrada, aes(x = V2010, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Proporção de Evasão por Cor/Raça',
       x = 'Cor/Raça',
       y = 'Proporção (%)',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()

## 1.2.3A Gráfico com Percentuais no Topo ####
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Calcular os percentuais de evasão por cor/raça
base_evasao_percentual_cor <- base_evasao_filtrada %>%
  group_by(V2010, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2010) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico com os percentuais no topo das barras
ggplot(base_evasao_percentual_cor, aes(x = V2010, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = paste0('Evasão por Cor/Raça com Percentuais no Topo - Acumulado: Período ', inicio, '-', fim),
       x = 'Cor/Raça',
       y = 'Frequência',
       fill = 'Evasão (1=Sim)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Melhor visualização

## 1.2.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Filtrar valores não nulos (NA) em V2010 e evasao
base_evasao_filtrada_validos <- base_evasao_filtrada %>%
  filter(!is.na(V2010) & !is.na(evasao))

# Calcular os percentuais de evasão por cor/raça
base_evasao_percentual_cor_validos <- base_evasao_filtrada_validos %>%
  group_by(V2010, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2010) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico com os percentuais no topo das barras (Sem NAs)
ggplot(base_evasao_percentual_cor_validos, aes(x = V2010, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = paste0('Evasão por Cor/Raça (Sem NAs) - Acumulado: Período ', inicio, '-', fim),
       x = 'Cor/Raça',
       y = 'Frequência',
       fill = 'Evasão (1=Sim)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.2.5A Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Criar uma tabela consolidada
tabela_cor_clean <- base_evasao_filtrada_validos %>%
  group_by(V2010, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2010) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Verificar e ajustar a tabela para remoção de valores irrelevantes
tabela_cor_clean <- tabela_cor_clean %>%
  filter(!is.na(V2010))  # Filtrar valores inválidos, se necessário

# Gerar título dinâmico para a tabela
titulo_dinamico <- paste0('Proporção de Evasão por Cor/Raça (Sem NAs) - Período: ', inicio, '-', fim)

# Exportar a tabela como HTML
tabela_html <- stargazer(
  tabela_cor_clean,
  type = 'html',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.2.1B Resumo Descritivo da Cor/Raça ####
# Dicionário para mapear os códigos para os nomes das categorias de Cor/Raça
mapa_cor_raca <- c(
  '1' = 'Branca',
  '2' = 'Preta',
  '3' = 'Amarela',
  '4' = 'Parda',
  '5' = 'Indígena',
  '9' = 'Ignorado'
)

# Aplicar o mapeamento e calcular os resultados
tabela_cor_raca_ano <- base_evasao_filtrada %>%
  mutate(V2010 = as.character(V2010)) %>% # Garantir que V2010 seja texto
  mutate(Cor_Raca = recode(V2010, !!!mapa_cor_raca)) %>% # Mapear os códigos
  group_by(Ano, Cor_Raca, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>% # Contar observações
  group_by(Ano, Cor_Raca) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>% # Calcular proporção
  ungroup()

# Renomear colunas para maior clareza
colnames(tabela_cor_raca_ano) <- c('Ano', 'Cor_Raca', 'Evasao', 'Contagem', 'Proporcao')

# Exportar a tabela com stargazer em HTML
titulo_dinamico <- 'Proporção de Evasão por Cor/Raça Segmentada por Ano'
tabela_html <- stargazer(
  tabela_cor_raca_ano,
  type = 'html',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))

## 1.2.2B Gráfico Inicial: Proporção de Evasão por Cor/Raça ####
# Gráfico mostrando a proporção de evasão por cor/raça para cada ano
ggplot(base_evasao_filtrada, aes(x = V2010, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Ano, ncol = 2) + # Facetar por ano
  labs(
    title = 'Proporção de Evasão por Cor/Raça - Separado por Ano',
    x = 'Cor/Raça',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.2.3B Gráfico com Percentuais no Topo ####
# Filtrar idades válidas (14-24 anos) e remover NAs na evasao
base_evasao_filtrada_validos_ano <- base_evasao_filtrada %>%
  filter(V2009 >= 14 & V2009 <= 24 & !is.na(evasao)) %>%
  mutate(Cor_Raca = recode(as.character(V2010), !!!mapa_cor_raca))

## 1.2.4B Gráfico com Percentuais no Topo (Sem NAs)***####
# Calcular os percentuais por cor/raça, evasao e ano
base_evasao_percentual_ano <- base_evasao_filtrada_validos_ano %>%
  group_by(Ano, Cor_Raca, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, Cor_Raca) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Gráfico com os percentuais no topo das barras
ggplot(base_evasao_percentual_ano, aes(x = Cor_Raca, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  facet_wrap(~Ano, ncol = 2) + # Facetar por ano
  labs(
    title = 'Proporção de Evasão por Cor/Raça com Percentuais no Topo - Separado por Ano',
    x = 'Cor/Raça',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.2.5B Exportação Final da Tabela (Sem NAs)** ####
# Consolidar a tabela final com proporções
tabela_cor_raca_df_clean_ano <- tabela_cor_raca_ano %>%
  filter(Cor_Raca != 'Ignorado' & !is.na(Cor_Raca))

# Exportar a tabela consolidada como HTML
tabela_html <- stargazer(
  tabela_cor_raca_df_clean_ano,
  type = 'html',
  summary = FALSE,
  title = 'Proporção de Evasão por Cor/Raça (Sem NAs) - Segmentada por Ano',
  digits = 2,
  rownames = FALSE
)

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## 1.3 SEXO ####

#### ////// (A) DADOS EMPILHADOS ////// ####
# 1.3.1A Resumo Descritivo de Sexo ####
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

# Gerar a tabela como HTML com stargazer
tabela_html <- stargazer(
  tabela_sexo,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = 'Proporção de Evasão por Sexo',
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

## 1.3.2A Gráfico Inicial: Proporção de Evasão por Sexo ####
ggplot(base_evasao_filtrada, aes(x = V2007, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Proporção de Evasão por Sexo',
       x = 'Sexo',
       y = 'Proporção (%)',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()

## 1.3.3A Gráfico com Percentuais no Topo ####
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

## 1.3.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
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

## 1.3.5A Exportação Final da Tabela (Sem NAs em Evasao)** ####
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
titulo_dinamico <- paste0('Proporção de Evasão por Sexo (Recalculada para cada Sexo) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.3.1B Resumo Descritivo de Sexo ####
# Criar um mapeamento para os valores de Sexo
mapa_sexo <- c(
  '1' = 'Homem',
  '2' = 'Mulher'
)

# Resumo estatístico: Contagem e proporção de evasão por sexo segmentada por ano
tabela_sexo_ano <- base_evasao_filtrada %>%
  mutate(V2007 = as.character(V2007)) %>%  # Converter para texto
  mutate(Sexo = recode(V2007, !!!mapa_sexo)) %>%  # Recode para 'Homem' e 'Mulher'
  group_by(Ano, Sexo, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, Sexo) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Renomear colunas para maior clareza
colnames(tabela_sexo_ano) <- c('Ano', 'Sexo', 'Evasao', 'Contagem', 'Proporcao')

# Exportar a tabela com stargazer em HTML
titulo_dinamico <- 'Proporção de Evasão por Sexo Segmentada por Ano'
tabela_html <- stargazer(
  tabela_sexo_ano,
  type = 'html',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))

## 1.3.2B Gráfico Inicial: Proporção de Evasão por Sexo ####
# Gráfico de proporção por sexo segmentada por ano
ggplot(base_evasao_filtrada, aes(x = V2007, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Proporção de Evasão por Sexo - Separado por Ano',
    x = 'Sexo',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.3.3B Gráfico com Percentuais no Topo ####
# Calcular os percentuais de evasão por sexo por ano
base_evasao_percentual_sexo_ano <- base_evasao_filtrada %>%
  group_by(Ano, V2007, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, V2007) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico com percentuais no topo, separado por ano
ggplot(base_evasao_percentual_sexo_ano, aes(x = V2007, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Proporção de Evasão por Sexo com Percentuais no Topo - Separado por Ano',
    x = 'Sexo',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.3.4B Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar valores não nulos (NA) em V2007 (Sexo) e evasao
base_evasao_filtrada_validos_sexo <- base_evasao_filtrada %>%
  filter(!is.na(V2007) & !is.na(evasao))

base_evasao_filtrada_validos_sexo

# Calcular os percentuais de evasão por sexo por ano
base_evasao_percentual_sexo_ano <- base_evasao_filtrada_validos_sexo %>%
  group_by(Ano, V2007, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, V2007) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

# Gráfico com percentuais no topo, separado por ano, sem NAs
ggplot(base_evasao_percentual_sexo_ano, aes(x = V2007, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Proporção de Evasão por Sexo (Sem NAs) com Percentuais no Topo - Separado por Ano',
    x = 'Sexo',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.3.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Criar um mapeamento para os valores de Sexo
mapa_sexo <- c(
  '1' = 'Homem',
  '2' = 'Mulher'
)

# Filtrar valores válidos (remover NAs de Sexo e Evasao)
tabela_sexo_clean <- base_evasao_filtrada %>%
  filter(!is.na(V2007) & !is.na(evasao)) %>%  # Remover NAs
  mutate(V2007 = as.character(V2007)) %>%  # Converter para texto
  mutate(Sexo = recode(V2007, !!!mapa_sexo)) %>%  # Recode para 'Homem' e 'Mulher'
  group_by(Ano, Sexo, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, Sexo) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Renomear as colunas para maior clareza
colnames(tabela_sexo_clean) <- c('Ano', 'Sexo', 'Evasao', 'Contagem', 'Proporcao')

# Criar um título dinâmico para a tabela
titulo_dinamico <- 'Proporção de Evasão por Sexo (Sem NAs) - Segmentada por Ano'

# Exportar a tabela com stargazer (HTML)
tabela_html <- stargazer(
  tabela_sexo_clean,
  type = 'html',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))



## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.4 REGIÃO ####

#### ////// (A) DADOS EMPILHADOS ////// ####

## 1.4.1A Resumo Descritivo da Região ####
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

# Gerar a tabela como HTML com stargazer
tabela_html <- stargazer(
  tabela_regiao,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = 'Proporção de Evasão por Região',
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))


## 1.4.2A Gráfico Inicial: Proporção de Evasão por Região ####
ggplot(base_evasao_filtrada, aes(x = regiao, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Proporção de Evasão por Região',
       x = 'Região',
       y = 'Proporção (%)',
       fill = 'Evasão (1=Sim)') +
  theme_minimal()


## 1.4.3A Gráfico com Percentuais no Topo ####
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

## 1.4.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
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

## 1.4.5A Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Filtrar valores válidos (remover NAs da variável Evasao) (ESTE!)
tabela_regiao_clean <- tabela_regiao %>%
  filter(!is.na(Evasao))  # Remove NAs apenas da coluna Evasao

# Verificar a tabela filtrada
print(tabela_regiao_clean)

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0('Proporção de Evasão por Região (Sem NAs em Evasao) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
# Cabeçalho inicial para mostrar a estrutura da base
head(base_evasao_filtrada, 2)

## 1.4.1B Resumo Descritivo da Região ####
# Calcular contagem e proporção de evasão por região e ano
tabela_regiao_ano <- base_evasao_filtrada %>%
  group_by(Ano, regiao, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, regiao) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Renomear colunas para maior clareza
colnames(tabela_regiao_ano) <- c('Ano', 'Regiao', 'Evasao', 'Contagem', 'Proporcao')

# Gerar o título dinâmico para a tabela
titulo_dinamico <- 'Proporção de Evasão por Região Segmentada por Ano'

# Exportar tabela com stargazer (HTML)
tabela_html <- stargazer(
  tabela_regiao_ano,
  type = 'html',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))

## 1.4.2B Gráfico Inicial: Proporção de Evasão por Região ####
# Gráfico mostrando a proporção de evasão por região para cada ano
ggplot(base_evasao_filtrada, aes(x = regiao, fill = as.factor(evasao))) +
  geom_bar(position = 'fill', color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Proporção de Evasão por Região - Separado por Ano',
    x = 'Região',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.4.3B Gráfico com Percentuais no Topo ####
# Calcular os percentuais de evasão por região e ano
base_evasao_percentual_regiao_ano <- base_evasao_filtrada %>%
  group_by(Ano, regiao, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, regiao) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Gráfico com percentuais no topo das barras
ggplot(base_evasao_percentual_regiao_ano, aes(x = regiao, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Evasão por Região com Percentuais no Topo - Separado por Ano',
    x = 'Região',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.4.4B Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar valores não nulos (NA) em regiao e evasao
base_evasao_filtrada_validos_ano <- base_evasao_filtrada %>%
  filter(!is.na(regiao) & !is.na(evasao))

# Calcular os percentuais de evasão por região e ano
base_evasao_percentual_regiao_validos_ano <- base_evasao_filtrada_validos_ano %>%
  group_by(Ano, regiao, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, regiao) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Gráfico com percentuais no topo das barras (Sem NAs)
ggplot(base_evasao_percentual_regiao_validos_ano, aes(x = regiao, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Evasão por Região (Sem NAs) com Percentuais no Topo - Separado por Ano',
    x = 'Região',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.4.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Consolidar a tabela final sem NAs
tabela_regiao_clean_ano <- tabela_regiao_ano %>%
  filter(!is.na(Regiao) & !is.na(Evasao))  # Remove NAs da coluna Evasao

# Exportar a tabRegiao# Exportar a tabela como HTML com stargazer
titulo_dinamico <- 'Proporção de Evasão por Região (Sem NAs) - Segmentada por Ano'
tabela_html <- stargazer(
  tabela_regiao_clean_ano,
  type = 'html',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.5 RDPC ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.5.1A Resumo Descritivo do RDPC ####
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

# Gerar a tabela como HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = 'Proporção de Evasão por Faixas de RDPC (Ajustadas pelo Salário Mínimo)',
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(html_output))

## 1.5.2A Gráfico Inicial: Proporção de Evasão por Faixas de RDPC ####
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

## 1.5.3A Gráfico com Percentuais no Topo ####
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

## 1.5.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
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
  'Evasão por Faixas de RDPC (Sem NAs) - Ajustadas pelo Salário Mínimo - Período: ',
  inicio, '-', fim
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


## 1.5.5A Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Filtrar tabela sem NAs em evasao
tabela_rdpc_clean <- tabela_rdpc %>%
  filter(!is.na(Evasao))  # Remove NAs apenas da coluna evasao

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0('Proporção de Evasão por Faixas de RDPC (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.5.1B Resumo Descritivo do RDPC ####
# Adicionar o salário mínimo à base, calculado para cada ano
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))  # Adiciona o salário mínimo correspondente ao ano

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo e RDPC
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & !is.na(RDPC) & Salario_Minimo > 0)

# Calcular contagem e proporção de evasão por faixas de RDPC e ano
tabela_rdpc_ano <- base_evasao_filtrada %>%
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
  group_by(Ano, Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Renomear colunas para maior clareza
colnames(tabela_rdpc_ano) <- c('Ano', 'Faixa_RDPC', 'Evasao', 'Contagem', 'Proporcao')

# Gerar o título dinâmico para a tabela
titulo_dinamico <- 'Proporção de Evasão por Faixas de RDPC Segmentada por Ano'

# Exportar tabela com stargazer (HTML)
tabela_html <- stargazer(
  tabela_rdpc_ano,
  type = 'html',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))

## 1.5.2B Gráfico Inicial: Proporção de Evasão por Faixas de RDPC ####
# Gráfico mostrando a proporção de evasão por faixas de RDPC para cada ano
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
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Proporção de Evasão por Faixas de RDPC - Separado por Ano',
    x = 'Faixa de RDPC',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.5.3B Gráfico com Percentuais no Topo ####
# Gráfico com percentuais no topo das barras
ggplot(tabela_rdpc_ano, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Proporcao, '%')),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Evasão por Faixas de RDPC com Percentuais no Topo - Separado por Ano',
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.5.4B Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar valores válidos (sem NAs em evasao e RDPC)
base_evasao_filtrada_validos_ano <- base_evasao_filtrada %>%
  filter(!is.na(evasao) & !is.na(RDPC))

# Calcular percentuais com categorias ajustadas
tabela_rdpc_validos_ano <- base_evasao_filtrada_validos_ano %>%
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
  group_by(Ano, Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Ano, Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Gráfico com percentuais no topo (sem NAs)
ggplot(tabela_rdpc_validos_ano, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Proporcao, '%')),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  facet_wrap(~Ano, ncol = 2) +  # Facetar por ano
  labs(
    title = 'Evasão por Faixas de RDPC (Sem NAs) com Percentuais no Topo - Separado por Ano',
    x = 'Faixa de RDPC',
    y = 'Frequência',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.5.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Consolidar a tabela final sem NAs
tabela_rdpc_clean_ano <- tabela_rdpc_ano %>%
  filter(!is.na(Faixa_RDPC) & !is.na(Evasao))  # Remove NAs das colunas relevantes

# Exportar a tabela como HTML com stargazer
titulo_dinamico <- 'Proporção de Evasão por Faixas de RDPC (Sem NAs) - Segmentada por Ano'
tabela_html <- stargazer(
  tabela_rdpc_clean_ano,
  type = 'html',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = '\n')))

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.6 RDPC POR REGIÃO ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.6.1A Resumo Descritivo do RDPC por Região ####
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

# Gerar a tabela como HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_regiao,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = 'Proporção de Evasão por Faixas de RDPC por Região (Ajustadas pelo Salário Mínimo)',
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(html_output))

## 1.6.2A Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Região ####
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

## 1.6.3A Gráfico com Percentuais no Topo ####
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

## 1.6.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
# Gráfico com percentuais no topo (sem NAs) por região, com texto rotacionado em 90 graus
# Criar tabela filtrada sem valores NA em evasao
tabela_rdpc_regiao_validos <- tabela_rdpc_regiao %>%
  filter(!is.na(Evasao))  # Remover apenas os valores NA em evasao

# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Evasão por Faixas de RDPC (Sem NAs) por Região - Período: ',
  inicio, '-', fim
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


## 1.6.5A Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Filtrar tabela sem NAs em evasao e regiao
tabela_rdpc_regiao_clean <- tabela_rdpc_regiao %>%
  filter(!is.na(Evasao) & !is.na(Regiao))  # Remove NAs

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0('Proporção de Evasão por Faixas de RDPC por Região (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.6.1B Resumo Descritivo do RDPC por Região ####
# Adicionar o salário mínimo à base
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo, RDPC e Região
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & !is.na(RDPC) & Salario_Minimo > 0 & !is.na(regiao))

# Calcular contagem e proporção de evasão por faixas de RDPC, região e ano
tabela_rdpc_regiao_ano <- base_evasao_filtrada %>%
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
  group_by(Ano, regiao, Faixa_RDPC, evasao) %>%
  summarise(Contagem = n(), .groups = "drop") %>%
  group_by(Ano, regiao, Faixa_RDPC) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2)) %>%
  ungroup()

# Renomear colunas para maior clareza
colnames(tabela_rdpc_regiao_ano) <- c("Ano", "Regiao", "Faixa_RDPC", "Evasao", "Contagem", "Proporcao")

# Gerar título dinâmico
titulo_dinamico <- "Proporção de Evasão por Faixas de RDPC por Região Segmentada por Ano"

# Exportar tabela como HTML
tabela_html <- stargazer(
  tabela_rdpc_regiao_ano,
  type = "html",
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = "\n")))

## 1.6.2B Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Região ####
# Gráfico inicial mostrando a proporção de evasão por faixas de RDPC para cada região e ano
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
  geom_bar(position = "fill", color = "black") +
  facet_wrap(~Ano + regiao, ncol = 2) +  # Facetar por região e ano
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proporção de Evasão por Faixas de RDPC por Região e Ano",
    x = "Faixa de RDPC",
    y = "Proporção (%)",
    fill = "Evasão (1=Sim)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.6.3B Gráfico com Percentuais no Topo ####
# Criar gráfico com percentuais no topo das barras segmentado por região e ano
ggplot(tabela_rdpc_regiao_ano, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(
    aes(label = paste0(Proporcao, "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~Ano + Regiao, ncol = 2) +  # Facetar por região e ano
  labs(
    title = "Evasão por Faixas de RDPC com Percentuais no Topo por Região e Ano",
    x = "Faixa de RDPC",
    y = "Frequência",
    fill = "Evasão (1=Sim)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.6.4B Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar valores válidos (sem NAs em evasao e regiao)
tabela_rdpc_regiao_validos_ano <- tabela_rdpc_regiao_ano %>%
  filter(!is.na(Evasao) & !is.na(Regiao))

# Gráfico com percentuais no topo (Sem NAs)
ggplot(tabela_rdpc_regiao_validos_ano, aes(x = Faixa_RDPC, y = Contagem, fill = as.factor(Evasao))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(
    aes(label = paste0(Proporcao, "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~Ano + Regiao, ncol = 2) +  # Facetar por região e ano
  labs(
    title = "Evasão por Faixas de RDPC (Sem NAs) com Percentuais no Topo por Região e Ano",
    x = "Faixa de RDPC",
    y = "Frequência",
    fill = "Evasão (1=Sim)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 1.6.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Consolidar a tabela final sem NAs
tabela_rdpc_regiao_clean_ano <- tabela_rdpc_regiao_ano %>%
  filter(!is.na(Faixa_RDPC) & !is.na(Evasao) & !is.na(Regiao))

# Gerar título dinâmico
titulo_dinamico <- "Proporção de Evasão por Faixas de RDPC (Sem NAs) por Região e Ano"

# Exportar a tabela como HTML
tabela_html <- stargazer(
  tabela_rdpc_regiao_clean_ano,
  type = "html",
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(paste(tabela_html, collapse = "\n")))

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.7 RDPC POR COR ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.7.1A Resumo Descritivo do RDPC por Cor ####
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

# Gerar a tabela como HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_cor,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = 'Proporção de Evasão por Faixas de RDPC por Cor (Ajustadas pelo Salário Mínimo)',
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(html_output))

## 1.7.2A Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Cor ####
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

## 1.7.3A Gráfico com Percentuais no Topo ####
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

## 1.7.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
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
  'Evasão por Faixas de RDPC (Sem NAs) por Cor - Período: ',
  inicio, '-', fim
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

## 1.7.5A Exportação Final da Tabela (Sem NAs em Evasao e Cor)** ####
# Filtrar tabela sem NAs em evasao e V2010
tabela_rdpc_cor_clean <- tabela_rdpc_cor %>%
  filter(!is.na(Evasao) & !is.na(Cor))  # Remove NAs

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0('Proporção de Evasão por Faixas de RDPC por Cor (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.7.1B Resumo Descritivo do RDPC por Cor** ####

## 1.7.2B Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Cor ####

## 1.7.3B Gráfico com Percentuais no Topo ####

## 1.7.4B Gráfico com Percentuais no Topo (Sem NAs)** ####

## 1.7.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.8 RDPC POR SEXO** ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.8.1A Resumo Descritivo do RDPC por Sexo ####
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

# Gerar a tabela como HTML com stargazer
tabela_html <- stargazer(
  tabela_rdpc_sexo,
  type = 'html',            # Exportar como HTML
  summary = FALSE,          # Sem resumo
  title = 'Proporção de Evasão por Faixas de RDPC por Sexo (Ajustadas pelo Salário Mínimo)',
  digits = 2                # Número de casas decimais
)

# Unir o vetor HTML em uma única string
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar a tabela no Viewer do RStudio
htmltools::html_print(HTML(html_output))

## 1.8.2A Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Sexo ####
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

## 1.8.3A Gráfico com Percentuais no Topo ####
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

## 1.8.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar dados válidos
tabela_rdpc_sexo_validos <- tabela_rdpc_sexo %>%
  filter(!is.na(Evasao))

# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Evasão por Faixas de RDPC (Sem NAs) por Sexo - Período: ',
  inicio, '-', fim
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


## 1.8.5A Exportação Final da Tabela (Sem NAs em Evasao e Sexo)** ####
tabela_rdpc_sexo_clean <- tabela_rdpc_sexo %>%
  filter(!is.na(Evasao) & !is.na(Sexo))  # Remove NAs de Evasao e Sexo

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0('Proporção de Evasão por Faixas de RDPC por Sexo (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.8.1B Resumo Descritivo do RDPC por Sexo** ####

## 1.8.2B Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Sexo ####

## 1.8.3B Gráfico com Percentuais no Topo ####

## 1.8.4B Gráfico com Percentuais no Topo (Sem NAs)** ####

## 1.8.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.9 RDPC POR ENSINO MÉDIO ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.9.1A Resumo Descritivo do RDPC por Ensino Médio** ####
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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

## 1.9.2A Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Ensino Médio ####
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

## 1.9.3A Gráfico com Percentuais no Topo  ####
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

## 1.9.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
# Filtrar dados válidos
tabela_rdpc_ensino_medio_validos <- tabela_rdpc_ensino_medio %>%
  filter(!is.na(Evasao))

# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Evasão por Faixas de RDPC (Sem NAs) por Ensino Médio - Período: ',
  inicio, '-', fim
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


## 1.9.5A Exportação Final da Tabela (Sem NAs em Evasao e Ensino Médio)** ####
tabela_rdpc_ensino_medio_clean <- tabela_rdpc_ensino_medio %>%
  filter(!is.na(Evasao) & !is.na(Ensino_Medio))  # Remove NAs de Evasao e Ensino Médio

# Gerar título dinâmico com as variáveis 'inicio' e 'fim', sem aspas
titulo_dinamico <- paste0('Proporção de Evasão por Faixas de RDPC por Ensino Médio (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.9.1B Resumo Descritivo do RDPC por Ensino Médio** ####

## 1.9.2B Gráfico Inicial: Proporção de Evasão por Faixas de RDPC por Ensino Médio ####

## 1.9.3B Gráfico com Percentuais no Topo ####

## 1.9.4B Gráfico com Percentuais no Topo (Sem NAs)** ####

## 1.9.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.10 RDPC POR EVASÃO ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.10.1A Resumo Descritivo do RDPC por Evasão** ####
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
titulo_dinamico <- paste0('Proporção de Evasão por Faixas de RDPC (Ajustadas pelo Salário Mínimo) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

## 1.10.2A Gráfico Inicial: Proporção de Evasão por Faixas de RDPC** ####
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Proporção de Evasão por Faixas de RDPC - Período: ',
  inicio, '-', fim
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

## 1.10.3A Gráfico com Percentuais no Topo** ####
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Evasão por Faixas de RDPC com Percentuais no Topo - Período: ',
  inicio, '-', fim
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

## 1.10.4A Gráfico com Percentuais no Topo (Sem NAs) ####
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Evasão por Faixas de RDPC (Sem NAs) - Período: ',
  inicio, '-', fim
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


## 1.10.5A Exportação Final da Tabela (Sem NAs em Evasao)** ####
# Gerar título dinâmico com as variáveis 'inicio' e 'fim'
titulo_dinamico <- paste0('Proporção de Evasão por Faixas de RDPC (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.10.1B Resumo Descritivo do RDPC por Evasão** ####

## 1.10.2B Gráfico Inicial: Proporção de Evasão por Faixas de RDPC** ####

## 1.10.3B Gráfico com Percentuais no Topo ####

## 1.10.4B Gráfico com Percentuais no Topo (Sem NAs)** ####

## 1.10.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.11 RDPC POR EVASÃO E ENSINO MÉDIO** ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.11.1A Resumo Descritivo do RDPC por Evasão e Ensino Médio** ####
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
titulo_dinamico <- paste0('Proporção de Evasão por Faixas de RDPC e Ensino Médio (Ajustadas pelo Salário Mínimo) - Período: ', inicio, '-', fim)

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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))


## 1.11.2A Gráfico Inicial: Proporção de Evasão por Faixas de RDPC e Ensino Médio ####
# Gráfico inicial: proporção de evasão por faixas de RDPC segmentado por ensino médio
# Não faz sentido

## 1.11.3A Gráfico com Percentuais no Topo ####
# Não faz sentido

## 1.11.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Evasão por Faixas de RDPC e Ensino Médio (Sem NAs) - Período: ',
  inicio, '-', fim
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


## 1.11.5A Exportação Final da Tabela (Sem NAs em Evasao e Ensino Médio)** #### 
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Proporção de Evasão por Faixas de RDPC e Ensino Médio (Sem NAs e Ajustadas pelo Salário Mínimo) - Período: ',
  inicio, '-', fim
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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.11.1B Resumo Descritivo do RDPC por Evasão e Ensino Médio** ####

## 1.11.2B Gráfico Inicial: Proporção de Evasão por Faixas de RDPC e Ensino Médio ####

## 1.11.3B Gráfico com Percentuais no Topo ####

## 1.11.4B Gráfico com Percentuais no Topo (Sem NAs)** ####

## 1.11.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.12 RESUMO EVASÃO ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.12.1A Resumo Descritivo da Evasão** ####
# Resumo descritivo da evasão
tabela_evasao <- base_evasao_filtrada %>%
  group_by(evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2))
tabela_evasao

## 1.12.2A Gráfico Inicial: Proporção de Evasão** #### 
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Proporção de Evasão - Período: ',
  inicio, '-', fim
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


## 1.12.3A Exportação Final da Tabela (Sem NAs em Evasao)** #### 
# Gerar título dinâmico com período
titulo_dinamico <- paste0(
  'Proporção de Evasão (Sem NAs) - Período: ',
  inicio, '-', fim
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
html_output <- paste(tabela_html, collapse = '\n')

# Renderizar no Viewer do RStudio
htmltools::html_print(HTML(html_output))

#### ////// (B) DADOS LONGITUDINAIS ////// ####
head(base_evasao_filtrada, 2)

## 1.12.1B Resumo Descritivo de Evasão** ####

## 1.12.2B Gráfico Inicial: Proporção de Evasão** ####

## 1.12.3B Exportação Final da Tabela (Sem NAs em Evasao)** ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####
## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
### 1.13 MOTIVOS DE EVASÃO ####
# - Análise preliminar e acessória de motivos de evasão, complementando a análise de evasão
# utilizando dados empilhados e longitudinais, a fim de identificar possíveis fatores associados
# à evasão escolar que poderão ser testados nos modelos probit, logit e heckit.

## PAREI AQUI (consertar abaixo VD4047)

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
  type = 'text',
  summary = FALSE,
  title = 'Motivos Financeiros: Evasão e Indicadores Econômicos',
  digits = 2
)

# Gráfico de renda per capita por evasão
ggplot(base_evasao_filtrada_validos, aes(x = as.factor(evasao), y = VD2003)) +
  geom_boxplot(fill = 'lightblue', color = 'darkblue') +
  labs(
    title = 'Renda Domiciliar Per Capita por Evasão',
    x = 'Evasão (1=Sim)',
    y = 'Renda Domiciliar Per Capita (VD2003)'
  ) +
  theme_minimal()

## 1.13.2 Falta de Infraestrutura ####
# Análise de evasão por localização (urbana/rural)
tabela_infraestrutura <- base_evasao_filtrada_validos %>%
  group_by(V1014, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V1014) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2))

# Exibir tabela formatada
stargazer(
  tabela_infraestrutura,
  type = 'text',
  summary = FALSE,
  title = 'Evasão por Localização (Urbano/Rural)',
  digits = 2
)

# Gráfico de evasão por localização
ggplot(tabela_infraestrutura, aes(x = V1014, y = Proporcao, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  labs(
    title = 'Evasão por Localização (Urbano/Rural)',
    x = 'Localização (1=Urbano, 2=Rural)',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.13.3 Necessidade de Trabalhar ####
# Jovens ocupados com carga de trabalho e evasão
base_evasao_trabalho <- base_evasao_filtrada_validos %>%
  mutate(
    Trabalha = ifelse(!is.na(V4001) & V4001 == 1, 'Sim', 'Não')
  )

tabela_trabalho_evasao <- base_evasao_trabalho %>%
  group_by(Trabalha, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(Trabalha) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2))

# Exibir tabela formatada
stargazer(
  tabela_trabalho_evasao,
  type = 'text',
  summary = FALSE,
  title = 'Evasão e Necessidade de Trabalhar',
  digits = 2
)

# Gráfico de evasão por carga de trabalho
ggplot(tabela_trabalho_evasao, aes(x = Trabalha, y = Proporcao, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  labs(
    title = 'Evasão por Necessidade de Trabalhar',
    x = 'Trabalha',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
  ) +
  theme_minimal()

## 1.13.4 Fatores Domésticos e Culturais ####
# Análise por sexo e arranjo familiar
tabela_domestico_cultural <- base_evasao_filtrada_validos %>%
  group_by(VD2002, V2007, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(VD2002, V2007) %>%
  mutate(Proporcao = round(Contagem / sum(Contagem) * 100, 2))

# Exibir tabela formatada
stargazer(
  tabela_domestico_cultural,
  type = 'text',
  summary = FALSE,
  title = 'Evasão por Arranjo Familiar e Sexo',
  digits = 2
)

# Gráfico de evasão por sexo e arranjo familiar
ggplot(tabela_domestico_cultural, aes(x = as.factor(VD2002), y = Proporcao, fill = as.factor(evasao))) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  facet_wrap(~V2007, labeller = labeller(V2007 = c('1' = 'Masculino', '2' = 'Feminino'))) +
  labs(
    title = 'Evasão por Arranjo Familiar e Sexo',
    x = 'Arranjo Familiar (VD2002)',
    y = 'Proporção (%)',
    fill = 'Evasão (1=Sim)'
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
  type = 'text',
  summary = FALSE,
  title = 'Transferências Sociais e Evasão',
  digits = 2
)

# Gráfico de valores médios de transferências sociais
ggplot(base_evasao_filtrada_validos, aes(x = as.factor(evasao), y = VD4047)) +
  geom_boxplot(fill = 'lightgreen', color = 'darkgreen') +
  labs(
    title = 'Transferências Sociais por Evasão',
    x = 'Evasão (1=Sim)',
    y = 'Transferências Sociais (VD4047)'
  ) +
  theme_minimal()




