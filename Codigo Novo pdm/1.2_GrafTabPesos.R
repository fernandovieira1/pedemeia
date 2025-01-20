### GRÁFICOS E TABELAS (COM PESOS)
# - Preparação da versão final, aplicando os pesos para a população e formatando o código tex.
gc()

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## 1.2.1A Resumo Descritivo da Cor/Raça ####

# Objeto da análise preliminar (amostral)
htmltools::html_print(a_tab_resumo_cor)


origem <- '%1.2.1A Resumo Descritivo da Cor/Raça'

## Pesos ####
# Desenho amostral
desenho <- svydesign(
  ids = ~UPA,                # Cluster (Unidade Primária de Amostragem)
  strata = ~Estrato,         # Estratificação
  weights = ~V1028032,          # Pesos amostrais ajustados
  data = base_evasao_filtrada,
  nest = TRUE                # Indica amostras aninhadas
)

# Ajustar os cálculos ponderados
tabela_cor_raca_ponderada <- svytable(~V2010 + evasao, design = desenho)
proporcao_ponderada <- prop.table(tabela_cor_raca_ponderada, margin = 1) * 100

# Converter em um data.frame para visualização
tabela_cor_raca_ponderada_df <- as.data.frame(as.table(proporcao_ponderada))
colnames(tabela_cor_raca_ponderada_df) <- c("V2010", "evasao", "Proporcao")

## Código original ####
# Resumo estatístico: Contagem e proporção de evasão por cor/raça
tabela_cor_raca <- tabela_cor_raca_ponderada_df %>%
  mutate(Proporcao = round(Proporcao, 2)) %>% 
  as.data.frame() 

# Filtrar apenas os índices de evasão
tabela_cor_raca <- tabela_cor_raca %>% 
  filter(evasao == 1) %>% 
  filter(V2010 != 'Ignorado') %>%
  arrange(desc(Proporcao)) %>% 
  select(-evasao)

# Renomear as colunas para evitar problemas com nomes vazios ou inválidos
colnames(tabela_cor_raca) <- c('Cor', '%')

# Formatar %
tabela_cor_raca <- tabela_cor_raca %>%
  mutate(`%` = paste0(format(`%`, nsmall = 2, decimal.mark = ",")))
tabela_cor_raca

# Gerar o título dinâmico com as variáveis 'inicio' e 'fim' sem aspas
titulo_dinamico <- paste0('Proporção de Evasão por Cor/Raça - Período: ', inicio, '-', fim)

# Exibir a tabela com stargazer (em HTML)
a_tab_resumo_cor_latex <- stargazer(
  tabela_cor_raca,
  type = 'latex',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

# Remover as linhas de comentário geradas pelo stargazer
a_tab_resumo_cor_latex <- gsub("^%.*$", "", a_tab_resumo_cor_latex)

# Adicionar uma formatação com indentação para exibição organizada
a_tab_resumo_cor_latex <- paste(a_tab_resumo_cor_latex, collapse = "\n")

# Exibir o resultado
a_tab_resumo_cor_latex <- paste0(
  origem,
  a_tab_resumo_cor_latex
)


## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.2.3A Gráfico com Percentuais no Topo ####
a_graf_cor_percentual 

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.3.3A Gráfico com Percentuais no Topo ####
a_graf_sexo_percentual

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.4.1A Resumo Descritivo da Região ####
htmltools::html_print(a_tab_resumo_regiao) 

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.4.3A Gráfico com Percentuais no Topo ####
a_graf_regiao_percentual 

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.7.1A Resumo Descritivo do RDPC por Cor ####
htmltools::html_print(a_tab_resumo_rdpc_cor)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.12.1A Resumo Descritivo ####
htmltools::html_print(a_tab_resumo_populacao) 

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

