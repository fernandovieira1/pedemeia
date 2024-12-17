library(data.table)
library(dplyr)
library(survey)
library(stringr)
library(tidyr)
#cadunico <- fread("C:/Users/helen/OneDrive/Documentos/FEA-RP/Eco II/base_amostra_pessoa_201812.csv")
dados_cadunico <- fread("C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia\\base_amostra_pessoa_201812.csv")
#cadunico <- fread("C:/Users/helen/OneDrive/Documentos/FEA-RP/Eco II/base_amostra_pessoa_201812.csv")

# Selecionando colunas principais
cadunico <- select(dados_cadunico, cd_ibge, cod_sexo_pessoa, idade, cod_parentesco_rf_pessoa, cod_raca_cor_pessoa,
                   ind_frequenta_escola_memb, cod_curso_frequenta_memb, cod_curso_frequentou_pessoa_memb,
                   cod_concluiu_frequentou_memb, cod_principal_trab_memb, val_remuner_emprego_memb,
                   val_outras_rendas_memb, peso.pes, id_familia, estrato, classf, cod_ano_serie_frequenta_memb)

# Transformação para urbano e rural
cadunico <- cadunico %>%
  mutate(classf = case_when(
    classf == 1 ~ "Urbano",
    classf == 2 ~ "Urbano",
    classf == 3 ~ "Rural",
    TRUE ~ NA_character_
  ))

# Transformações de região e país
cadunico <- cadunico %>%
  mutate(cod_ibge = paste0(cd_ibge),
         Pais = factor("Brasil"),
         GR = case_when(
           startsWith(cod_ibge, "1") ~ "Norte",
           startsWith(cod_ibge, "2") ~ "Nordeste",
           startsWith(cod_ibge, "3") ~ "Sudeste",
           startsWith(cod_ibge, "4") ~ "Sul",
           startsWith(cod_ibge, "5") ~ "Centro-Oeste",
           TRUE ~ NA_character_
         ))

# Faixas etárias
cadunico$fx_idade <- factor(
  ifelse(cadunico$idade >= 14 & cadunico$idade <= 19, "14 a 19 anos",
         ifelse(cadunico$idade >= 20 & cadunico$idade <= 24, "20 a 24 anos", NA)),
  levels = c("14 a 19 anos", "20 a 24 anos")
)
## DÚVIDA FERNANDO: Por que esses intervalos?

# Variável escolaridade dos pais
# Agrupamento por família e identificação de mãe e pai
cadunico <- cadunico %>%
  group_by(id_familia) %>%
  mutate(is_mae = cod_sexo_pessoa == 2 & cod_parentesco_rf_pessoa %in% c(1, 2) & cod_concluiu_frequentou_memb == 1,
         is_pai = cod_sexo_pessoa == 1 & cod_parentesco_rf_pessoa %in% c(1, 2) & cod_concluiu_frequentou_memb == 1,
         educacao_mae = ifelse(any(is_mae), cod_curso_frequentou_pessoa_memb[is_mae][1], NA),
         educacao_pai = ifelse(any(is_pai), cod_curso_frequentou_pessoa_memb[is_pai][1], NA),
         max_educacao_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-is_mae, -is_pai)
## DÚVIDA FERNANDO: Comparar com PNAD_Final.R

# Mapeamento de códigos para níveis educacionais
cadunico$max_educacao_pais <- factor(cadunico$max_educacao_pais, 
                                     levels = 1:15, 
                                     labels = c("Creche", "Pré-escola (exceto CA)", "Classe de Alfabetização - CA",
                                                "Ensino Fundamental 1ª a 4ª séries", "Ensino Fundamental 5ª a 8ª séries",
                                                "Ensino Fundamental (9 anos)", "Ensino Fundamental Especial",
                                                "Ensino Médio", "Ensino Médio Especial",
                                                "Ensino Fundamental EJA - séries iniciais",
                                                "Ensino Fundamental EJA - séries finais", "Ensino Médio EJA",
                                                "Superior, Aperfeiçoamento", "Alfabetização para Adultos", "Nenhum"))
## DÚVIDA FERNANDO: Comparar com PNAD_Final.R

# Transformação dos códigos de trabalho e raça
cadunico$cod_principal_trab_memb <- factor(cadunico$cod_principal_trab_memb, 
                                           levels = 1:11,
                                           labels = c("Trabalhador por conta própria", "Trabalhador temporário rural", 
                                                      "Empregado sem carteira", "Empregado com carteira", 
                                                      "Doméstico sem carteira", "Doméstico com carteira", 
                                                      "Trabalhador não-remunerado", "Militar ou servidor público", 
                                                      "Empregador", "Estagiário", "Aprendiz"))
## DÚVIDA FERNANDO: Comparar com PNAD_Final.R

cadunico$cod_raca_cor_pessoa <- factor(cadunico$cod_raca_cor_pessoa, 
                                       levels = 1:5, 
                                       labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
## DÚVIDA FERNANDO: Comparar com PNAD_Final.R

# Transformação do código de sexo
cadunico$cod_sexo_pessoa <- factor(cadunico$cod_sexo_pessoa, 
                                   levels = 1:2, labels = c("Masculino", "Feminino"))
## DÚVIDA FERNANDO: Comparar com PNAD_Final.R

# Cálculo da renda domiciliar per capita
cadunico <- cadunico %>%
  mutate(val_remuner_emprego_memb = replace_na(val_remuner_emprego_memb, 0),
         val_outras_rendas_memb = replace_na(val_outras_rendas_memb, 0),
         renda_total = val_remuner_emprego_memb + val_outras_rendas_memb) %>%
  group_by(id_familia) %>%
  mutate(renda_total_familia = sum(renda_total, na.rm = TRUE),
         numero_pessoas_familia = n(),
         renda_per_capita = renda_total_familia / numero_pessoas_familia) %>%
  ungroup()
## DÚVIDA FERNANDO: Comparar com PNAD_Final.R

# Filtrar e calcular jovens do público-alvo

salario_minimo <- 954
jovens_publico_alvo <- cadunico %>%
  filter(idade >= 14 & idade <= 24 & ind_frequenta_escola_memb == 1 & renda_per_capita <= (0.5 * salario_minimo))

total_jovens_publico_alvo <- nrow(jovens_publico_alvo)
## DÚVIDA FERNANDO: Comparar com PNAD_Final.R

# Cálculos de Estatísticas Descritivas para o Público-Alvo

# Calcular média de idade
media_idade <- mean(jovens_publico_alvo$idade, na.rm = TRUE)

# Calcular mediana de idade
mediana_idade <- median(jovens_publico_alvo$idade, na.rm = TRUE)

# Calcular desvio padrão da idade
desvio_padrao_idade <- sd(jovens_publico_alvo$idade, na.rm = TRUE)

# Exibir média, mediana e desvio padrão
media_idade
mediana_idade
desvio_padrao_idade

# Frequência de cada idade
frequencia_idade <- table(jovens_publico_alvo$idade)

# Porcentagem de cada idade
porcentagem_idade <- prop.table(frequencia_idade) * 100

# Exibir frequências e porcentagens de idade
frequencia_idade
porcentagem_idade

# Criar tabela com frequências e porcentagens de idade
tabela_idade <- as.data.frame(frequencia_idade) %>%
  rename(Idade = Var1, Frequencia = Freq) %>%
  mutate(Porcentagem = Frequencia / sum(Frequencia) * 100)

# Exibir a tabela de idade
print(tabela_idade)

# SEXO

# Frequência de cada sexo
frequencia_sexo <- table(jovens_publico_alvo$cod_sexo_pessoa)

# Porcentagem de cada sexo
porcentagem_sexo <- prop.table(frequencia_sexo) * 100

# Exibir frequências e porcentagens de sexo
frequencia_sexo
porcentagem_sexo

# Criar tabela com frequências e porcentagens de sexo
tabela_sexo <- as.data.frame(frequencia_sexo) %>%
  rename(Sexo = Var1, Frequencia = Freq) %>%
  mutate(Porcentagem = Frequencia / sum(Frequencia) * 100)

# Exibir a tabela de sexo
print(tabela_sexo)

# COR #

# Calcular a frequência de cor ou raça
frequencia_cor <- table(jovens_publico_alvo$cod_raca_cor_pessoa)

# Transformar a frequência em uma tabela e renomear as colunas
tabela_cor <- as.data.frame(frequencia_cor, stringsAsFactors = FALSE)
names(tabela_cor) <- c("Cor", "Frequencia")

# Calcular a porcentagem para cada cor ou raça
tabela_cor <- tabela_cor %>%
  mutate(Porcentagem = Frequencia / sum(Frequencia) * 100)

# Exibir a tabela
print(tabela_cor)

# Estatísticas para região (GR)
frequencia_regiao <- table(jovens_publico_alvo$GR)
tabela_regiao <- as.data.frame(frequencia_regiao) %>%
  rename(Regiao = Var1, Frequencia = Freq) %>%
  mutate(Porcentagem = Frequencia / sum(Frequencia) * 100)

print(tabela_regiao)

# Calcular a frequência de cada classificação de 'urbano' e 'rural' no público-alvo
frequencia_urbano_rural <- table(jovens_publico_alvo$classf)

# Calcular a porcentagem de cada classificação de 'urbano' e 'rural'
porcentagem_urbano_rural <- prop.table(frequencia_urbano_rural) * 100

# Exibir os resultados
frequencia_urbano_rural
porcentagem_urbano_rural

# Transformar a frequência em uma tabela para visualização
tabela_urbano_rural <- as.data.frame(frequencia_urbano_rural) %>%
  rename(Urbano_Rural = Var1, Frequencia = Freq) %>%
  mutate(Porcentagem = Frequencia / sum(Frequencia) * 100)

# Exibir a tabela
print(tabela_urbano_rural)

# Calculando a frequência de cada nível de escolaridade dos pais
frequencia_educacao_pais <- table(jovens_publico_alvo$max_educacao_pais)

# Transformando a frequência em um data frame e calculando a porcentagem
tabela_educacao_pais <- as.data.frame(frequencia_educacao_pais, stringsAsFactors = FALSE) %>%
  rename(EducacaoPais = Var1, Frequencia = Freq) %>%
  mutate(Porcentagem = Frequencia / sum(Frequencia) * 100)

# Exibindo a tabela descritiva de educação dos pais
print(tabela_educacao_pais)

# Calcular a variável de atraso escolar
cadunico_atraso_escolar <- jovens_publico_alvo %>%
  mutate(
    atraso = ifelse(!is.na(cod_ano_serie_frequenta_memb) & idade > cod_ano_serie_frequenta_memb, 1, 0)
  )

# Cálculo da renda domiciliar per capita
cadunico <- cadunico %>%
  mutate(val_remuner_emprego_memb = replace_na(val_remuner_emprego_memb, 0),
         val_outras_rendas_memb = replace_na(val_outras_rendas_memb, 0),
         renda_total = val_remuner_emprego_memb + val_outras_rendas_memb) %>%
  group_by(id_familia) %>%
  mutate(renda_total_familia = sum(renda_total, na.rm = TRUE),
         numero_pessoas_familia = n(),
         renda_per_capita = renda_total_familia / numero_pessoas_familia) %>%
  ungroup()

# Filtrar para o público-alvo com base no filtro original
salario_minimo <- 954  # Salário mínimo atual
jovens_publico_alvo <- cadunico %>%
  filter(idade >= 14 & idade <= 24, 
         ind_frequenta_escola_memb == 1,  # Frequentando escola
         renda_per_capita <= (0.5 * salario_minimo))  # Renda per capita até meio salário mínimo


# Definindo a data de referência (31/03)
data_referencia <- as.Date("2024-03-31")  # Exemplo de data para comparação

# Filtrando os dados para o público-alvo
cadunico_publico_alvo <- cadunico %>%
  filter(idade >= 14 & idade <= 24,  # Considerando a faixa etária do público-alvo
         ind_frequenta_escola_memb == 1,  # Frequenta escola pública (considerando que 1 é 'sim')
         val_remuner_emprego_memb + val_outras_rendas_memb <= 0.5 * salario_minimo)  # Renda até meio salário mínimo

# Calculando o atraso apenas para o público-alvo
cadunico_publico_alvo$atraso <- ifelse(
  cadunico_publico_alvo$idade >= 18 & cadunico_publico_alvo$idade < 19,  # Exemplo de faixa etária para 1º ano
  ifelse(cadunico_publico_alvo$idade %% 2 == 0, "atrasado", "nao_atrasado"),  # Exemplo de critério
  "sem_informacao"
)

## DÚVIDA FERNANDO: A condição cadunico_publico_alvo$idade %% 2 == 0 verifica se a idade 
# é par, mas essa verificação em si não determina diretamente se um estudante está 
# "atrasado"?

# Exibindo o resultado
table(cadunico_publico_alvo$atraso)  # Contagem dos atrasos

# Filtrando apenas os alunos atrasados do público-alvo
atrasados_publico_alvo <- cadunico_publico_alvo %>%
  filter(atraso == "atrasado")  # Considerando que 'atraso == 1' indica os alunos atrasados

# Estatísticas descritivas para os atrasados dentro do público-alvo
descritivas_atrasados <- atrasados_publico_alvo %>%
  summarise(
    n = n(),  # Contagem de alunos atrasados
    media_idade = mean(idade, na.rm = TRUE),  # Média de idade dos atrasados
    mediana_idade = median(idade, na.rm = TRUE),  # Mediana de idade dos atrasados
    sd_idade = sd(idade, na.rm = TRUE),  # Desvio padrão da idade dos atrasados
    media_renda = mean(val_remuner_emprego_memb + val_outras_rendas_memb, na.rm = TRUE),  # Média de renda dos atrasados
    mediana_renda = median(val_remuner_emprego_memb + val_outras_rendas_memb, na.rm = TRUE),  # Mediana de renda dos atrasados
    sd_renda = sd(val_remuner_emprego_memb + val_outras_rendas_memb, na.rm = TRUE)  # Desvio padrão da renda dos atrasados
  )

# Exibindo as descritivas dos atrasados
print(descritivas_atrasados)

# Calcular a porcentagem de pessoas atrasadas no público-alvo
percent_atrasado <- cadunico_publico_alvo %>%
  filter(atraso == "atrasado") %>%
  summarise(percentual = n() / nrow(cadunico_publico_alvo) * 100)

# Exibir o percentual
percent_atrasado

# Calcular o tamanho do domicílio e unipessoalidade
composicao_domicilio <- cadunico_publico_alvo %>%
  group_by(id_familia) %>%
  summarise(
    tamanho_domicilio = n(),
    unipessoal = ifelse(n() == 1, 1, 0),  # Se for unipessoal (1 pessoa no domicílio)
    filhos = sum(cod_parentesco_rf_pessoa == 1, na.rm = TRUE),   # Considera 1 como filho
    pais = sum(cod_parentesco_rf_pessoa == 2, na.rm = TRUE),     # Considera 2 como pai
    cônjuges = sum(cod_parentesco_rf_pessoa == 3, na.rm = TRUE)  # Considera 3 como cônjuge
  ) %>%
  ungroup()

# Exibir as estatísticas gerais de composição de domicílio
composicao_domicilio_descritivas <- composicao_domicilio %>%
  summarise(
    media_tamanho_domicilio = mean(tamanho_domicilio),
    mediana_tamanho_domicilio = median(tamanho_domicilio),
    prop_unipessoal = mean(unipessoal),  # Proporção de domicílios unipessoais
    media_filhos = mean(filhos),
    media_pais = mean(pais),
    media_conjuges = mean(cônjuges)
  )

# Exibir a tabela com a composição do domicílio
composicao_domicilio_descritivas



