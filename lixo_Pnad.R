### 2.5 Público Pé-de-Meia #### DEPOIS
# Critérios e dfs necessários para a análise
#    * MEC: https://www.gov.br/mec/pt-br/pe-de-meia/publico
# pdm = Pé-de-Meia

# *Sem pesos (pdms) ####
# pdms <- pnad %>%
#   filter(
#     (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
#    # V3002 == 'Sim',                                # Frequentam a escola
#    # V3002A == 'Rede pública',                      # Rede pública de ensino
#     V3003A %in% c('Regular do ensino médio',      # Estudantes do ensino médio regular
#                   'Educação de jovens e adultos (EJA) do ensino médio'),
#     (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),       # Rendimento domiciliar per capita até metade do salário mínimo, ou
#     V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
#     VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
#   ) %>%
#   mutate(contagem = 1) # Variável para contar as observações
# summary(pdms) # 396 obs.

pdms <- pnad %>%
  filter(
    V2009 >= 14 & V2009 <= 24 &           # Idade entre 14 e 24 anos
      V3002 == "Sim" &                      # Frequenta escola
      V3003A == "Regular do ensino médio" & # Ensino médio regular
      V3002A == "Rede pública" &            # Escola pública
      (RDPC <= sal_min_ap / 2 |             # Renda per capita até ½ salário mínimo
         V5001A == "Sim" | V5002A == "Sim" | V5003A == "Sim") & # Benefício social
      VD2004 != "Unipessoal"                # Domicílios não unipessoais
  ) %>%
  mutate(contagem = 1) # Variável para contar as observações
summary(pdms) # 4732 obs.

# pdms <- pnad %>%
#   filter(
#     (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
#     (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),      # Rendimento domiciliar per capita até metade do salário mínimo, ou
#     V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
#     VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
#   ) %>%
#   mutate(contagem = 1) # Variável para contar as observações
# summary(pdms) # 4037 obs.

# *Com pesos (pdmc) ####
pdmc <- svydesign(
  ids = ~1,                    # IDs sem clusterização
  weights = ~V1032,            # Pesos calibrados da PNAD
  data = 
    pdms <- pnad %>%
    filter(
      V2009 >= 14 & V2009 <= 24 &           # Idade entre 14 e 24 anos
        V3002 == "Sim" &                      # Frequenta escola
        V3003A == "Regular do ensino médio" & # Ensino médio regular
        V3002A == "Rede pública" &            # Escola pública
        (RDPC <= sal_min_ap / 2 |             # Renda per capita até ½ salário mínimo
           V5001A == "Sim" | V5002A == "Sim" | V5003A == "Sim") & # Benefício social
        VD2004 != "Unipessoal"                # Domicílios não unipessoais
    ) %>%
    mutate(contagem = 1) # Variável para contar as observações
) 
summary(pdmc)

## ****Por Região (GR) ####
pdmc_gr <- as.data.frame(svytotal(~factor(GR), 
                                  pdmc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(pdmc_gr)
sum(pdmc_gr$total) # 2.125.892 alunos entre 14 e 24 anos elegíveis pdm

## ****Por Estado (UF) ####
pdmc_uf <- as.data.frame(svytotal(~factor(UF), 
                                  pdmc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(pdmc_uf)
sum(pdmc_uf$total) # 2.125.892 alunos entre 14 e 24 anos elegíveis pdm

## *Notas 2.5 ####
# - ~2.1 mi de alunos elegíveis para o Pé-de-Meia
# - Valores coerentes com a realidade
# - Fontes:
#    * O Globo (~2 mi em 2023): https://oglobo.globo.com/economia/educacao/programa-pe-de-meia-para-estudantes-de-ensino-medio-de-familias-pobres-deve-custar-71-bilhao-em-2024-25436668
#    * Ag. Brasil (~2.7 mi em 2024): https://agenciabrasil.ebc.com.br/educacao/noticia/2024-10/pe-de-meia-pagamento-estudantes-comeca-nesta-segunda-feira#:~:text=O%20governo%20federal%20calcula%20que,ser%20sacado%20em%20qualquer%20momento.

====================================
### 2.5 Público Pé-de-Meia #### ANTES
# Critérios e dfs necessários para a análise
# pdm = Pé-de-Meia

# *Sem pesos (pdms) ####
# pdms <- pnad %>%
#   filter(
#     (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
#    # V3002 == 'Sim',                                # Frequentam a escola
#    # V3002A == 'Rede pública',                      # Rede pública de ensino
#     V3003A %in% c('Regular do ensino médio',      # Estudantes do ensino médio regular
#                   'Educação de jovens e adultos (EJA) do ensino médio'),
#     (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),       # Rendimento domiciliar per capita até metade do salário mínimo, ou
#     V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
#     VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
#   ) %>%
#   mutate(contagem = 1) # Variável para contar as observações
# summary(pdms) # 396 obs.

pdms <- pnad %>%
  filter(
    (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
    (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),      # Rendimento domiciliar per capita até metade do salário mínimo, ou
    V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
    VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
  ) %>%
  mutate(contagem = 1) # Variável para contar as observações
summary(pdms) # 4037 obs.

# *Com pesos (pdmc) ####
pdmc <- svydesign(
  ids = ~1,                    # IDs sem clusterização
  weights = ~V1032,            # Pesos calibrados da PNAD
  data = 
    pdms <- pnad %>%
    filter(
      (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
      (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),      # Rendimento domiciliar per capita até metade do salário mínimo, ou
      V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
      VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
    ) %>%
    mutate(contagem = 1) # Variável para contar as observações
) 
summary(pdmc)

## ****Por Região (GR) ####
pdmc_gr <- as.data.frame(svytotal(~factor(GR), 
                                  pdmc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(pdmc_gr)
sum(pdmc_gr$total) # 2.057.804 alunos entre 14 e 24 anos elegíveis pdm

## ****Por Estado (UF) ####
pdmc_uf <- as.data.frame(svytotal(~factor(UF), 
                                  pdmc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(pdmc_uf)
sum(pdmc_uf$total) # 2.057.804 alunos entre 14 e 24 anos elegíveis pdm

## *Notas 2.5 ####
# - ~2 mi de alunos elegíveis para o Pé-de-Meia
# - Valores coerentes com a realidade
# - Fontes:
#    * O Globo (~2 mi em 2023): https://oglobo.globo.com/economia/educacao/programa-pe-de-meia-para-estudantes-de-ensino-medio-de-familias-pobres-deve-custar-71-bilhao-em-2024-25436668
#    * Ag. Brasil (~2.7 mi em 2024): https://agenciabrasil.ebc.com.br/educacao/noticia/2024-10/pe-de-meia-pagamento-estudantes-comeca-nesta-segunda-feira#:~:text=O%20governo%20federal%20calcula%20que,ser%20sacado%20em%20qualquer%20momento.

# - Verificar se os filtros fazem sentido

================================
### 2.4 Público Ensino Médio ####
# em: ensino médio

# *Sem pesos (ems) ####
# Retirei o filtro de excluir famílias unipessoais
ems <- pnad %>%
  filter(
    (V2009 >= 14 & V2009 <= 24),                 # Idade entre 14 e 24 anos
    #V3002 == 'Sim',                               # Frequentam a escola
    V3003A %in% c('Regular do ensino médio',      # Estudantes do ensino médio regular
                  'Educação de jovens e adultos (EJA) do ensino médio')
  ) %>%
  mutate(contagem = 1) # Variável para contar as observações
summary(ems) # 15.183 obs.

# *Com pesos (emc) ####
# Retirei o filtro de excluir famílias unipessoais
emc <- svydesign(
  ids = ~1,                    # IDs sem clusterização
  weights = ~V1032,            # Pesos calibrados da PNAD
  data = pnad %>%
    filter(
      (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
      #V3002 == 'Sim',                                # Frequentam a escola
      V3003A %in% c('Regular do ensino médio',      # Estudantes do ensino médio regular
                    'Educação de jovens e adultos (EJA) do ensino médio')
    ) %>%
    mutate(contagem = 1)                  # Dados carregados e transformados em tibble
) 
summary(emc) 

## ****Por Região (GR) ####
emc_gr <- as.data.frame(svytotal(~factor(GR), 
                                 emc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(emc_gr)
sum(emc_gr$total) # 8.3 mi de estudantres de ensino médio entre 14 e 24 anos

## ****Por Estado (UF) ####
emc_uf <- as.data.frame(svytotal(~factor(UF), 
                                 emc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(emc_uf)

sum(emc_uf$total) # 8.3 mi de estudantres de ensino médio entre 14 e 24 anos

## *Notas 2.4 ####
# - Curti: Público Potencial (10KK-16KK?)
# - Estimativa aqui: 8.3 mi
# - Segundo a Fiocruz, em 2023 a estimativa era de que havia 7.7 mi de 
# estudantes de ensino médio no Brasil. Fonte: https://www.epsjv.fiocruz.br/noticias/reportagem/censo-escolar-revela-queda-de-150-mil-matriculas-no-ensino-medio-em-2023#:~:text=Em%202023%2C%20foram%20registradas%207,aprova%C3%A7%C3%A3o%20no%20per%C3%ADodo%20da%20pandemia%E2%80%9D.
# - O valor encontrado aqui está próximo deste número.

==============================
# Configurando o design amostral com a coluna de peso
pnad_design <- svydesign(
  ids = ~1,                   # Colocando ~1 se não houver clusterização (ou substitua pelo identificador de cluster)
  weights = ~V1032,           # Coluna de peso calibrado
  data = pnad                 # Dataframe com os dados da PNAD
)

# Média ponderada de RDPC
media_rdpc <- svymean(~RDPC, design = pnad_design, na.rm = TRUE)
print(media_rdpc)

# Total ponderado de beneficiários de programas sociais
total_beneficiarios <- svytotal(~V5002A, design = pnad_design, na.rm = TRUE)
print(total_beneficiarios)

# Proporção ponderada por categoria de renda per capita
prop_rdpc_categoria <- svymean(~RDPC_categoria, design = pnad_design, na.rm = TRUE)
print(prop_rdpc_categoria)

# Média de RDPC por região e UF
media_por_regiao_uf <- svyby(
  ~RDPC,
  by = ~GR + UF,
  design = pnad_design,
  FUN = svymean,
  na.rm = TRUE
)
print(media_por_regiao_uf)

# Quantis ponderados de RDPC
quantis_rdpc <- svyquantile(
  ~RDPC, 
  design = pnad_design, 
  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
  ci = TRUE,
  na.rm = TRUE
)
print(quantis_rdpc)

# Quantis ponderados de RDPC
quantis_rdpc <- svyquantile(
  ~RDPC, 
  design = pnad_design, 
  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
  ci = TRUE,
  na.rm = TRUE
)
print(quantis_rdpc)

#############
# Código anterior 2.5
## *Beneficiários Pé-de-Meia ####
pdm <- pnad %>%
  filter(
    (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
    V3002 == 'Sim',                                # Frequentam a escola
    V3003A == 'Regular do ensino médio',           # Estudantes do ensino médio regular
    V3002A == 'Rede pública',                      # Rede pública de ensino
    (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),       # Rendimento domiciliar per capita até metade do salário mínimo, ou
    V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
    VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
  ) %>%
  mutate(contagem = 1) # Variável para contar as observações

# *Entre 14 e 24 anos que recebem alguma transferência de renda ####
# as: assistência social
pnadas <- pnad %>%
  filter(
    (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
    (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),      # Rendimento domiciliar per capita até metade do salário mínimo, ou
    V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
    VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
  ) %>%
  mutate(contagem = 1) # Variável para contar as observações

# Rendimento médio mensal domiciliar a preços médios do ano
rendimento_domiciliar_media_proprioano <- svyby(
  ~VD4016AP,                   # Rendimento habitual ajustado pelo ano presente
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnad_design,
  FUN = svymean,
  na.rm = TRUE
)
print(rendimento_domiciliar_media_proprioano)

# Rendimento médio mensal real domiciliar per capita a preços médios do ano
rendimento_domiciliar_per_capita_media_proprioano <- svyby(
  ~RDPC,                       # Rendimento domiciliar per capita
  by = ~GR + UF,
  design = pnad_design,
  FUN = svymean,
  na.rm = TRUE
)
print(rendimento_domiciliar_per_capita_media_proprioano)

# Total de pessoas nas faixas de rendimento domiciliar per capita
faixa_rendimento_domiciliar_total_proprioano <- svyby(
  ~RDPC_categoria,
  by = ~GR + UF,
  design = pnad_design,
  FUN = svytotal,
  na.rm = TRUE
)
print(faixa_rendimento_domiciliar_total_proprioano)

### Definindo Subconjuntos para Beneficiários

# Subconjunto beneficiários Pé-de-Meia (14 a 24 anos, critérios de renda e escolaridade)
pdm_beneficiarios <- subset(
  pnad, 
  V2009 >= 14 & V2009 <= 24 &           # Idade entre 14 e 24 anos
    V3002 == "Sim" &                      # Frequenta escola
    V3003A == "Regular do ensino médio" & # Ensino médio regular
    V3002A == "Rede pública" &            # Escola pública
    (RDPC <= sal_min_ap / 2 |             # Renda per capita até ½ salário mínimo
       V5001A == "Sim" | V5002A == "Sim" | V5003A == "Sim") & # Benefício social
    VD2004 != "Unipessoal"                # Domicílios não unipessoais
)
pdm_beneficiarios <- transform(pdm_beneficiarios, contagem = 1)

# Design amostral para o subconjunto
pdm_design <- svydesign(
  ids = ~1,
  weights = ~V1032,
  data = pdm_beneficiarios
)

# Estimativa da população de beneficiários (contagem ponderada)
contagem_beneficiados <- svyby(
  ~contagem,
  by = ~GR + UF,
  design = pdm_design,
  FUN = svytotal,
  na.rm = TRUE
)
contagem_beneficiados <- as.data.frame(contagem_beneficiados)
names(contagem_beneficiados)[1] <- "Regiao_UF"
print(contagem_beneficiados)

## Estimativa custo anual: 
# R$ 3.200 ano por aluno: R$ 1800 (frequência) + R$ 200 (matrícula) + R$ 1000 (conclusão) + R$ 200 (enem)
sum(contagem_beneficiados$contagem)*3200 # ~R$ 6.8 bi

# Criando uma variável de contagem específica para cada ano
var_name1 <- paste0("contagem_", ano)
assign(var_name1, svyby(
  formula = ~contagem,
  by = ~GR + UF,
  design = pdm_design,
  FUN = svytotal,
  na.rm = TRUE
))
ano_contagem <- as.data.frame(get(var_name1))
print(get(var_name1))
