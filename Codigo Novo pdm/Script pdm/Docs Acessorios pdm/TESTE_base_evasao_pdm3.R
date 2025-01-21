
base_evasao_pdm3 <- base_evasao_filtrada %>%
  filter(
    !is.na(evasao) & !is.na(RDPC),
    V2009 >= 14 & V2009 <= 24
  ) %>%
  mutate(contagem = 1)

base_evasao_filtrada %>%
  select(evasao, RDPC, V2009, V3002, V3002A, V3003A, VD2004) %>%
  summary()

## i) Filtrar o dataframe para o ano de 2023 ####
base_teste <- subset(base_evasao_pdm3, Ano == 2023)

## ii) Criar o design amostral ####
options(survey.lonely.psu = "adjust")

design <- svydesign(
  ids = ~1,                     
  strata = ~Estrato,
  weights = ~V1028032,           
  data = base_teste,
  nest = TRUE                    
)

## iii) Adicionar estrutura para estimativas de renda, se necessário ####
design <- convey_prep(design)

## iv) Estimar o total da população-alvo ####
populacao_total <- svytotal(~ensino_medio, design)

## v) Exibir os resultados ####
print(populacao_total)



