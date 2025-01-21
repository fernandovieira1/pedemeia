base_evasao_pdm4 <- base_evasao_filtrada %>%
  filter(
    V2009 >= 14 & V2009 <= 24,
    V3003A %in% c("Regular do ensino médio", "EJA", "Técnico")
  ) %>%
  mutate(contagem = 1)





## i) Filtrar o dataframe para o ano de 2023 ####
base_teste <- subset(base_evasao_pdm4, Ano == 2023)

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



