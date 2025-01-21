library(data.table)
library(dplyr)
library(survey)
library(stringr)
library(tidyr)
library(readr)

# Carregar a base pessoa
#pessoas <- fread("C:/Users/admin/Desktop/base_amostra_cad_201812/base_amostra_pessoa_201812.csv"")
pessoas <- fread("C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia\\base_amostra_pessoa_201812.csv")
#pessoas <- fread("C:/Users/admin/Desktop/base_amostra_cad_201812/base_amostra_pessoa_201812.csv")

# file.exists("/Users/ivyszermeta/Library/Mobile Documents/com~apple~CloudDocs/PPGE - Economia/Bases/base_amostra_cad_201812/base_amostra_pessoa_201812.csv")

# Carregar a base família
#familia <- fread("C:/Users/admin/Desktop/base_amostra_cad_201812/base_amostra_familia_201812.csv")
familia <- fread("C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia\\base_amostra_familia_201812.csv")
#familia <- fread("C:/Users/admin/Desktop/base_amostra_cad_201812/base_amostra_familia_201812.csv")

base_fusionada <- inner_join(pessoas, familia, by = "id_familia")


# Criando variáveis auxiliares para obtenção da estimativa desejada
base_fusionada  <- transform(base_fusionada , cd_ibge.x=paste0(cd_ibge.x))
base_fusionada  <- transform(base_fusionada , Pais=as.factor("Brasil"))
base_fusionada$Pais <- factor(x=base_fusionada$Pais, levels=c("Brasil"))
base_fusionada  <- transform(base_fusionada , GR=as.factor(ifelse(substr(cd_ibge.x, start=1, stop=1)=="1","Norte",ifelse(substr(cd_ibge.x, start=1, stop=1)=="2","Nordeste",ifelse(substr(cd_ibge.x, start=1, stop=1)=="3","Sudeste",ifelse(substr(cd_ibge.x, start=1, stop=1)=="4","Sul",ifelse(substr(cd_ibge.x, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
base_fusionada$GR <- factor(x=base_fusionada$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))
base_fusionada <- transform(base_fusionada, fx_idade=as.factor(ifelse(idade>=14 & idade<=18,"14 a 18 anos",
                                                                      ifelse(idade>=19 & idade<=24,
                                                                             "19 a 24 anos",NA))))
base_fusionada$fx_idade <- factor(x=base_fusionada$fx_idade, levels=c("14 a 18 anos","19 a 24 anos"))


# Mother's education
base_fusionada <- base_fusionada %>%
  # Passo 1: Identificar a mãe em cada família
  mutate(is_mae = (cod_sexo_pessoa == 2 & (cod_parentesco_rf_pessoa == 1 | cod_parentesco_rf_pessoa == 2) &
                     cod_concluiu_frequentou_memb == 1)) %>%
  
  # Passo 2: Agrupar pela família e criar a coluna educacao_mae
  group_by(id_familia) %>%
  mutate(educacao_mae = ifelse(any(is_mae), cod_curso_frequentou_pessoa_memb[is_mae][1], NA)) %>%
  
  # Passo 3: Remover a coluna auxiliar is_mae e desagrupar
  select(-is_mae) %>%
  ungroup()

# Father Educ and Maximimum between Mother and Father
base_fusionada <- base_fusionada %>%
  # Passo 1: Identificar a mãe em cada família
  mutate(is_pai = (cod_sexo_pessoa == 1 & (cod_parentesco_rf_pessoa == 1 | cod_parentesco_rf_pessoa == 2) &
                     cod_concluiu_frequentou_memb == 1)) %>%
  
  # Passo 2: Agrupar pela família e criar a coluna educacao_pai
  group_by(id_familia) %>%
  mutate(educacao_pai = ifelse(any(is_pai), cod_curso_frequentou_pessoa_memb[is_pai][1], NA)) %>%
  
  # Passo 3: Criar a coluna max_educacao_pais com o valor máximo entre educacao_mae e educacao_pai
  mutate(max_educacao_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)) %>%
  
  # Passo 4: Remover a coluna auxiliar is_pai e desagrupar
  select(-is_pai) %>%
  ungroup()



base_fusionada <- transform(base_fusionada, max_educacao_pais=as.factor(ifelse(max_educacao_pais==1,"Creche",
                                                                               ifelse(max_educacao_pais==2,"Pré-escola (exceto CA)",
                                                                                      ifelse(max_educacao_pais==3,"Classe de Alfabetização - CA",
                                                                                             ifelse(max_educacao_pais==4,"Ensino Fundamental 1ª a 4ª séries, Elementar (Primário), Primeira fase do 1º grau",
                                                                                                    ifelse(max_educacao_pais==5,"Ensino Fundamental 5ª a 8ª séries, Médio 1º ciclo (Ginasial), Segunda fase do 1º grau",
                                                                                                           ifelse(max_educacao_pais==6,"Ensino Fundamental (duração 9 anos)",
                                                                                                                  ifelse(max_educacao_pais==7,"Ensino Fundamental Especial",
                                                                                                                         ifelse(max_educacao_pais==8,"Ensino Médio, 2º grau, Médio 2º ciclo (Científico, Clássico, Técnico, Normal)",
                                                                                                                                ifelse(max_educacao_pais==9,"Ensino Médio Especial", 
                                                                                                                                       ifelse(max_educacao_pais==10,"Ensino Fundamental EJA - séries iniciais (Supletivo 1ª a 4ª)",
                                                                                                                                              ifelse(max_educacao_pais==11,"Ensino Fundamental EJA - séries finais (Supletivo 5ª a 8ª)",
                                                                                                                                                     ifelse(max_educacao_pais==12,"Ensino Médio EJA (Supletivo)",
                                                                                                                                                            ifelse(max_educacao_pais==13,"Superior, Aperfeiçoamento, Especialização, Mestrado, Doutorado",
                                                                                                                                                                   ifelse(max_educacao_pais==14,"Alfabetização para Adultos (Mobral, etc.)",
                                                                                                                                                                          ifelse(max_educacao_pais==15,"Nenhum", NA)))))))))))))))))


base_fusionada$max_educacao_pais <- factor(x=base_fusionada$max_educacao_pais, levels=c("Creche","Pré-escola (exceto CA)",
                                                                                        "Classe de Alfabetização - CA","Ensino Fundamental 1ª a 4ª séries, Elementar (Primário), Primeira fase do 1º grau",
                                                                                        "Ensino Fundamental 5ª a 8ª séries, Médio 1º ciclo (Ginasial), Segunda fase do 1º grau","Ensino Fundamental (duração 9 anos)",
                                                                                        "Ensino Fundamental Especial","Ensino Médio, 2º grau, Médio 2º ciclo (Científico, Clássico, Técnico, Normal)",
                                                                                        "Ensino Médio Especial","Ensino Fundamental EJA - séries iniciais (Supletivo 1ª a 4ª)",
                                                                                        "Ensino Fundamental EJA - séries finais (Supletivo 5ª a 8ª)","Ensino Médio EJA (Supletivo)",
                                                                                        "Superior, Aperfeiçoamento, Especialização, Mestrado, Doutorado","Alfabetização para Adultos (Mobral, etc.)",
                                                                                        "Nenhum"))


# Tipo de emprego
base_fusionada <- transform(base_fusionada, cod_principal_trab_memb=as.factor(ifelse(cod_principal_trab_memb==1,"Trabalhador por conta própria (bico, autônomo)",
                                                                                     ifelse(cod_principal_trab_memb==2,"Trabalhador temporário em área rural",
                                                                                            ifelse(cod_principal_trab_memb==3,"Empregado sem carteira de trabalho assinada",
                                                                                                   ifelse(cod_principal_trab_memb==4,"Empregado com carteira de trabalho assinada",
                                                                                                          ifelse(cod_principal_trab_memb==5,"Trabalhador doméstico sem carteira de trabalho assinada",
                                                                                                                 ifelse(cod_principal_trab_memb==6,"Trabalhador doméstico com carteira de trabalho assinada",
                                                                                                                        ifelse(cod_principal_trab_memb==7,"Trabalhador não-remunerado",
                                                                                                                               ifelse(cod_principal_trab_memb==8,"Militar ou servidor público",
                                                                                                                                      ifelse(cod_principal_trab_memb==9,"Empregador", 
                                                                                                                                             ifelse(cod_principal_trab_memb==10,"Estagiário",
                                                                                                                                                    ifelse(cod_principal_trab_memb==11,"Aprendiz",
                                                                                                                                                           NA)))))))))))))
base_fusionada$cod_principal_trab_memb <- factor(x=base_fusionada$cod_principal_trab_memb, levels=c("Trabalhador por conta própria (bico, autônomo)",
                                                                                                    "Trabalhador temporário em área rural",
                                                                                                    "Empregado sem carteira de trabalho assinada",
                                                                                                    "Empregado com carteira de trabalho assinada",
                                                                                                    "Trabalhador doméstico sem carteira de trabalho assinada",
                                                                                                    "Trabalhador doméstico com carteira de trabalho assinada",
                                                                                                    "Trabalhador não-remunerado",
                                                                                                    "Militar ou servidor público",
                                                                                                    "Empregador","Estagiário","Aprendiz"))


# Raça
base_fusionada <- transform(base_fusionada, cod_raca_cor_pessoa=as.factor(ifelse(cod_raca_cor_pessoa==1,"Branca",
                                                                                 ifelse(cod_raca_cor_pessoa==2,"Preta",
                                                                                        ifelse(cod_raca_cor_pessoa==3,"Amarela",
                                                                                               ifelse(cod_raca_cor_pessoa==4,"Parda",
                                                                                                      ifelse(cod_raca_cor_pessoa==5,"Indígena",
                                                                                                             NA)))))))

base_fusionada$cod_raca_cor_pessoa <- factor(x=base_fusionada$cod_raca_cor_pessoa, levels=c("Branca",
                                                                                            "Preta",
                                                                                            "Amarela",
                                                                                            "Parda",
                                                                                            "Indígena"))

# Sexo
base_fusionada<- transform(base_fusionada, cod_sexo_pessoa=as.factor(ifelse(cod_sexo_pessoa==1,"Masculino",
                                                                            ifelse(cod_sexo_pessoa==2,"Feminino",
                                                                                   NA))))

base_fusionada$cod_sexo_pessoa <- factor(x=base_fusionada$cod_sexo_pessoa, levels=c("Masculino","Feminino"))


# Região
base_fusionada<- transform(base_fusionada, cod_local_domic_fam=as.factor(ifelse(cod_local_domic_fam==1,"Urbana",
                                                                                ifelse(cod_local_domic_fam==2,"Rural",
                                                                                       NA))))

base_fusionada$cod_local_domic_fam <- factor(x=base_fusionada$cod_local_domic_fam, levels=c("Urbana","Rural"))


# Bolsa família
base_fusionada<- transform(base_fusionada, marc_pbf=as.factor(ifelse(marc_pbf==1,"Sim",
                                                                     ifelse(marc_pbf==0,"Não",
                                                                            NA))))

base_fusionada$marc_pbf <- factor(x=base_fusionada$marc_pbf, levels=c("Sim","Não"))


# Adjusting geobr data for merging
#municipios$cd_ibge <- municipios$code_muni
#municipios$code_state <- as.numeric(municipios$code_state)
#municipios = subset(municipios, select= c(cd_ibge,code_state))
#municipios <- st_drop_geometry(municipios)
#estados = subset(estados, select =c(code_state,abbrev_state,name_state,name_region))
#estados <- st_drop_geometry(estados)


#base_merged <- merge(cadunico, municipios, by = "cd_ibge")
#base_merged <- merge(base_merged, estados, by = c("code_state"))

# Aplicar as transformações
base_fusionada <- base_fusionada %>%
  mutate(peso.pes = str_pad(peso.pes, 15, side = "right", pad = "0"), # Preenchendo a string
         peso.pes = as.numeric(peso.pes) * 1e-14)  # Convertendo para numérico e multiplicando



svy_design <- svydesign(
  id = ~id_familia,                # coluna para unidade primária de amostragem (PSU)
  strata = ~estrato.x,        # coluna para o estrato
  weights = ~peso.pes,          # coluna para os pesos amostrais
  data = base_fusionada,              # o dataframe de dados
  nest = TRUE               # se é um desenho aninhado (múltiplos estágios)
)

# Transformando para adicionar variável de contagem
svy_design <- transform(svy_design, contagem = 1)

# Aplicando os filtros adicionais - Ensino Médio
filtered_ensino_medio <- base_fusionada %>%
  filter(
    ind_frequenta_escola_memb == 1,
    # cod_curso_frequenta_memb == 7,
    idade >= 14 & idade <= 24,
    vlr_renda_media_fam <= 477,
  )
