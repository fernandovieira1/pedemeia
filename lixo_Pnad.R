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
