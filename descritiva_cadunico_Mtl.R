library(data.table)
library(dplyr)
library(survey)
library(stringr)
library(tidyr)
# Escolha do Ano a Analisar
#cadunico <- fread("C:/Users/rosan/OneDrive/Área de Trabalho/Eco II/Desafio/Desafio Eco II/Base/base_amostra_pessoa_201812.csv")
cadunico <- fread("C:/Users/rosan/OneDrive/Área de Trabalho/Eco II/Desafio/Desafio Eco II/Base/base_amostra_pessoa_201712_20190131.csv")
#cadunico <- fread("C:/Users/rosan/OneDrive/Área de Trabalho/Eco II/Desafio/Desafio Eco II/Base/base_amostra_pessoa_201612_20190131.csv")
ano <- 2017
cadunico <- select(cadunico,cd_ibge,cod_sexo_pessoa,idade,cod_parentesco_rf_pessoa,cod_raca_cor_pessoa,
                   ind_frequenta_escola_memb,cod_curso_frequenta_memb,cod_curso_frequentou_pessoa_memb,
                   cod_concluiu_frequentou_memb,cod_principal_trab_memb,val_remuner_emprego_memb,val_outras_rendas_memb,
                   peso.pes,id_familia,estrato)
# Criando variáveis auxiliares para obtenção da estimativa desejada
cadunico <- transform(cadunico, cod_ibge=paste0(cd_ibge))
cadunico <- transform(cadunico, Pais=as.factor("Brasil"))
cadunico$Pais <- factor(x=cadunico$Pais, levels=c("Brasil"))
cadunico <- transform(cadunico, GR=as.factor(ifelse(substr(cod_ibge, start=1, stop=1)=="1","Norte",ifelse(substr(cod_ibge, start=1, stop=1)=="2","Nordeste",ifelse(substr(cod_ibge, start=1, stop=1)=="3","Sudeste",ifelse(substr(cod_ibge, start=1, stop=1)=="4","Sul",ifelse(substr(cod_ibge, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
cadunico$GR <- factor(x=cadunico$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))
cadunico <- transform(cadunico, fx_idade=as.factor(ifelse(idade>=14 & idade<=19,"14 a 19 anos",
                                                          ifelse(idade>=20 & idade<=24,
                                                                 "20 a 24 anos",NA))))
cadunico$fx_idade <- factor(x=cadunico$fx_idade, levels=c("14 a 19 anos","20 a 24 anos"))

# Mother's education
cadunico <- cadunico %>%
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
cadunico <- cadunico %>%
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

cadunico <- transform(cadunico, max_educacao_pais=as.factor(ifelse(max_educacao_pais==1,"Creche",
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

cadunico$max_educacao_pais <- factor(x=cadunico$max_educacao_pais, levels=c("Creche","Pré-escola (exceto CA)",
                                                                            "Classe de Alfabetização - CA","Ensino Fundamental 1ª a 4ª séries, Elementar (Primário), Primeira fase do 1º grau",
                                                                            "Ensino Fundamental 5ª a 8ª séries, Médio 1º ciclo (Ginasial), Segunda fase do 1º grau","Ensino Fundamental (duração 9 anos)",
                                                                            "Ensino Fundamental Especial","Ensino Médio, 2º grau, Médio 2º ciclo (Científico, Clássico, Técnico, Normal)",
                                                                            "Ensino Médio Especial","Ensino Fundamental EJA - séries iniciais (Supletivo 1ª a 4ª)",
                                                                            "Ensino Fundamental EJA - séries finais (Supletivo 5ª a 8ª)","Ensino Médio EJA (Supletivo)",
                                                                            "Superior, Aperfeiçoamento, Especialização, Mestrado, Doutorado","Alfabetização para Adultos (Mobral, etc.)",
                                                                            "Nenhum"))

# Tipo de emprego
cadunico <- transform(cadunico, cod_principal_trab_memb=as.factor(ifelse(cod_principal_trab_memb==1,"Trabalhador por conta própria (bico, autônomo)",
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

cadunico$cod_principal_trab_memb <- factor(x=cadunico$cod_principal_trab_memb, levels=c("Trabalhador por conta própria (bico, autônomo)",
                                                                                        "Trabalhador temporário em área rural",
                                                                                        "Empregado sem carteira de trabalho assinada",
                                                                                        "Empregado com carteira de trabalho assinada",
                                                                                        "Trabalhador doméstico sem carteira de trabalho assinada",
                                                                                        "Trabalhador doméstico com carteira de trabalho assinada",
                                                                                        "Trabalhador não-remunerado",
                                                                                        "Militar ou servidor público",
                                                                                        "Empregador","Estagiário","Aprendiz"))

# Raça
cadunico <- transform(cadunico, cod_raca_cor_pessoa=as.factor(ifelse(cod_raca_cor_pessoa==1,"Branca",
                                                                     ifelse(cod_raca_cor_pessoa==2,"Preta",
                                                                            ifelse(cod_raca_cor_pessoa==3,"Amarela",
                                                                                   ifelse(cod_raca_cor_pessoa==4,"Parda",
                                                                                          ifelse(cod_raca_cor_pessoa==5,"Indígena",
                                                                                                 NA)))))))

cadunico$cod_raca_cor_pessoa <- factor(x=cadunico$cod_raca_cor_pessoa, levels=c("Branca",
                                                                                "Preta",
                                                                                "Amarela",
                                                                                "Parda",
                                                                                "Indígena"))

# Sexo
cadunico <- transform(cadunico, cod_sexo_pessoa=as.factor(ifelse(cod_sexo_pessoa==1,"Masculino",
                                                                 ifelse(cod_sexo_pessoa==2,"Feminino",
                                                                        NA))))

cadunico$cod_sexo_pessoa <- factor(x=cadunico$cod_sexo_pessoa, levels=c("Masculino","Feminino"))


# Renda Domiciliar Per Capita
cadunico <- cadunico %>%
  # Passo 1: Somar Renda do trabalho com outras rendas de cada indivíduo
  # Substituir NA na coluna renda por zero e calcular a renda total da família
  mutate(val_remuner_emprego_memb = replace_na(val_remuner_emprego_memb, 0)) %>%
  mutate(val_outras_rendas_memb = replace_na(val_outras_rendas_memb, 0)) %>%
  
  mutate(renda_total = (val_remuner_emprego_memb + val_outras_rendas_memb)) %>%
  
  group_by(id_familia) %>%
  
  # Calcular a renda total por família
  mutate(renda_total_familia = sum(renda_total, na.rm = TRUE),
         
         # Calcular o número de pessoas na família
         numero_pessoas_familia = n(),
         
         # Calcular a renda per capita
         renda_per_capita = renda_total_familia / numero_pessoas_familia) %>%
  
  # Desagrupar
  ungroup()

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
if (ano == 2018) {
  cadunico <- cadunico %>%
    mutate(peso.pes = str_pad(peso.pes, 15, side = "right", pad = "0"), # Preenchendo a string
           peso.pes = as.numeric(peso.pes) * 1e-14)  # Convertendo para numérico e multiplicando
} 

if (ano == 2016 | ano == 2017) {
  cadunico$peso.pes <- gsub(",", ".", cadunico$peso.pes) # substituindo , por .
  cadunico <- cadunico %>%
    mutate(peso.pes = as.numeric(peso.pes))  # Convertendo para numérico e multiplicando
  
}

svy_design <- svydesign(
  id = ~id_familia,                # coluna para unidade primária de amostragem (PSU)
  strata = ~estrato,        # coluna para o estrato
  weights = ~peso.pes,          # coluna para os pesos amostrais
  data = cadunico,              # o dataframe de dados
  nest = TRUE               # se é um desenho aninhado (múltiplos estágios)
)


# Variable for Counting Observations
svy_design <- transform(svy_design, contagem=1)
svy_design <- subset(svy_design, ind_frequenta_escola_memb==1 & cod_curso_frequenta_memb == 7 &
                       idade >= 14 & idade <= 24)



# Population of Beneficiaries Estimation
#print(survey::svytotal(x=~contagem, design=svy_design, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

print(survey::svybys(formula=~contagem, design=svy_design, bys = ~Pais+GR, FUN = svytotal,
                     vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Escolha a variável abaixo
#cod_sexo_pessoa+cod_raca_cor_pessoa+max_educacao_pais+renda_per_capita+cod_principal_trab_memb

print(svybys(formula = ~renda_per_capita, 
             by = ~Pais + GR, 
             design = svy_design, 
             FUN= svymean, 
             #vartype = c("se","cv"),
             keep.names = FALSE,
             na.rm = TRUE))

########################################################## PARAR AQUI ###################################################################

# Cálculos e Armazenamento para Loop
# Estimativa de indivíduos
var_name1 <- paste0("contagem_", ano)

assign(var_name1, svybys(
  formula = ~contagem,
  by = ~Pais + GR,
  design = svy_design, # Obtenha o design do survey para cada ano
  FUN = svytotal,
  vartype = c("se", "cv"),
  keep.names = FALSE,
  na.rm = TRUE
))

numeros <- c(1,2,3)

for (x in numeros) {
  var_name1A <- paste0(var_name1,"_",x)
  
  assign(var_name1A, as.data.frame(get(var_name1)[[x]]))
}

rm(get(var_name1))
# Estimativa de estats
# Definindo as variáveis para o loop
vars <- c(cod_sexo_pessoa,cod_raca_cor_pessoa,max_educacao_pais,renda_per_capita,cod_principal_trab_memb)
for (x in vars){
  var_name2 <- paste0("estats_", ano, "_", x)
  
  
  assign(var_name2, svybys(
    formula = ~cod_sexo_pessoa+cod_raca_cor_pessoa+max_educacao_pais+renda_per_capita+cod_principal_trab_memb,
    by = ~Pais + GR,
    design = svy_design, # Obtenha o design do survey para cada ano
    FUN = svymean,
    vartype = c("se", "cv"),
    keep.names = FALSE,
    na.rm = TRUE
  ))
  
  numeros <- c(1,2,3)
  
  for (x in numeros) {
    var_name2A <- paste0(var_name2,"_",x)
    
    assign(var_name2A, as.data.frame(get(var_name2)[[x]]))
  }
  rm(get(var_name2))
}


#for (ano in anos) {
#  for (x in numeros) {
#    var_name5 <- paste0("contagem_",ano)
#    var_name5A <- paste0(var_name5,"_",x)
#    var_name6 <- paste0("estats_",ano)
#    var_name6A <- paste0(var_name6,"_",x)
#   var_name7 <- paste0("estats_hab",ano)
#    var_name7A <- paste0(var_name7,"_",x)

#    assign(var_name5A, as.data.frame(get(var_name5)[[x]]))
#    assign(var_name6A, as.data.frame(get(var_name6)[[x]]))
#   if (ano != 2023) {
#      assign(var_name7A, as.data.frame(get(var_name7)[[x]]))
#   }

#  }
#}

#print(survey::svybys(formula=~contagem, design=svy_design, bys = ~Pais+GR, FUN = svytotal vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))