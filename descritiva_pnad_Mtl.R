##########################################################################
rm(list=ls(all=TRUE))
library(xtable)
library(dplyr)
# Definindo os anos para o loop
anos <- c(2016,2017,2018,2019,2022,2023)
#ano <- 2017 # Selecione um único ano para rodar
# Lista temporária para armazenar estatísticas para cada variável
estatisticas <- list()
estatisticas_hab <- list()
# Loop para cada ano
for (ano in anos) {  
  
  # Definindo limite de memória para compilação do programa
  aviso <- getOption("warn")
  options(warn=-1)
  memory.limit(size=50000)
  options(warn=aviso)
  rm(aviso)
  
  # Definindo tempo de espera para obtenção de resposta do servidor
  aviso <- getOption("warn")
  options(warn=-1)
  options(timeout=600)
  options(warn=aviso)
  rm(aviso)
  
  # Definindo opção de codificação dos caracteres e linguagem
  aviso <- getOption("warn")
  options(warn=-1)
  options(encoding="latin1")
  options(warn=aviso)
  rm(aviso)
  
  # Definindo opção de exibição de números sem representação em exponencial
  aviso <- getOption("warn")
  options(warn=-1)
  options(scipen=999)
  options(warn=aviso)
  rm(aviso)
  
  # Definindo opção de repositório para instalação dos pacotes necessários
  aviso <- getOption("warn")
  options(warn=-1)
  options(repos=structure(c(CRAN="https://cran.r-project.org/")))
  options(warn=aviso)
  rm(aviso)
  
  # Definindo diretório de trabalho
  caminho <- getwd()
  setwd(dir=caminho)
  
  # Carregando pacotes necessários para obtenção da estimativa desejada
  if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="PNADcIBGE", dependencies=TRUE)
  }
  library(package="PNADcIBGE", verbose=TRUE)
  if("dplyr" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="dplyr", dependencies=TRUE)
  }
  library(package="dplyr", verbose=TRUE)
  if("tibble" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="tibble", dependencies=TRUE)
  }
  library(package="tibble", verbose=TRUE)
  if("survey" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="survey", dependencies=TRUE)
  }
  library(package="survey", verbose=TRUE)
  if("convey" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="convey", dependencies=TRUE)
  }
  library(package="convey", verbose=TRUE)
  
  # Verificando manual de utilização da função para obtenção dos microdados
  help(topic="get_pnadc", package="PNADcIBGE")
  
  # Obtendo microdados anuais por visita da PNAD Contínua (PNADcIBGE >= 0.6.0)
  if (ano == 2023) {
    pnadc_anual_visita <- PNADcIBGE::get_pnadc(year=ano, interview=1, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                                               vars=c("UPA","V1008","V1014","Ano","Trimestre","UF","Capital","RM_RIDE","V2005",
                                                      "V2007","V2009","V2010","V3002","V3002A","V3003A","VD2006","VD2004","VD4019",
                                                      "VD4048","VD3005","VD4001","VD4002","VD4003","VD4014","VD4009","VD4020","VD4047","V5001A","V5002A","V5003A"))
  } else {                             
    pnadc_anual_visita <- PNADcIBGE::get_pnadc(year=ano, interview=1, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                                               vars=c("UPA","V1008","V1014","Ano","Trimestre","UF","Capital","RM_RIDE","V2005",
                                                      "V2007","V2009","V2010","V3002","V3002A","V3003A","VD2006","VD2004","VD4019",
                                                      "VD4048","VD3005","VD4001","VD4002","VD4003","VD4014","VD4009","VD4020","VD4047","V5001A","V5002A","V5003A",
                                                      "S01001","S01002","S01003","S01004","S01005","S01006","S01007A","S01010","S01011A","S01013",
                                                      "S010141","S01024","S01028","S010311","S010312"))
  }
  # Realizando coleta de lixo acumulada durante a obtenção dos microdados
  gc(verbose=FALSE, reset=FALSE, full=TRUE)
  
  # Criando variáveis auxiliares para obtenção da estimativa desejada
  pnadc_anual_visita <- transform(pnadc_anual_visita, ID_DOMICILIO=paste0(UPA,V1008,V1014))
  pnadc_anual_visita <- transform(pnadc_anual_visita, Pais=as.factor("Brasil"))
  pnadc_anual_visita$Pais <- factor(x=pnadc_anual_visita$Pais, levels=c("Brasil"))
  pnadc_anual_visita <- transform(pnadc_anual_visita, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
  pnadc_anual_visita$GR <- factor(x=pnadc_anual_visita$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))
  
  # Realizando processo de obtenção da estimativa do rendimento domiciliar real
  pnadc_anual_visita <- transform(pnadc_anual_visita, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_proprioano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO1))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4020real_proprioano=ifelse(is.na(VD4020) | is.na(V2001_rendimento),NA,VD4020*CO1e))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_proprioano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO1e))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_ultimoano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO2))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4020real_ultimoano=ifelse(is.na(VD4020) | is.na(V2001_rendimento),NA,VD4020*CO2e))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_ultimoano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO2e))
  pnadc_anual_visita_rendimento <- pnadc_anual_visita %>% dplyr::group_by(ID_DOMICILIO) %>% dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                                                                                                             rendimento_todos_trabalhos_proprioano=sum(VD4019real_proprioano, na.rm=TRUE),
                                                                                                             rendimento_todos_trabalhos_ef_proprioano=sum(VD4020real_proprioano, na.rm=TRUE),
                                                                                                             rendimento_outras_fontes_proprioano=sum(VD4048real_proprioano, na.rm=TRUE),
                                                                                                             rendimento_todos_trabalhos_ultimoano=sum(VD4019real_ultimoano, na.rm=TRUE),
                                                                                                             rendimento_outras_fontes_ultimoano=sum(VD4048real_ultimoano, na.rm=TRUE))
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, moradores_rendimento=moradores_rendimento)  
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano+rendimento_outras_fontes_proprioano)
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_ef_proprioano=rendimento_todos_trabalhos_ef_proprioano+rendimento_outras_fontes_proprioano)
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_ef_proprioano=VD5007real_ef_proprioano/moradores_rendimento)
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_ultimoano=rendimento_todos_trabalhos_ultimoano+rendimento_outras_fontes_ultimoano)
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_ultimoano=VD5007real_ultimoano/moradores_rendimento)
  pnadc_anual_visita <- pnadc_anual_visita[,!(names(pnadc_anual_visita) %in% c("V2001_rendimento","VD4019real_proprioano","VD4020real_proprioano","VD4048real_proprioano","VD4019real_ultimoano","VD4020real_ultimoano","VD4048real_ultimoano"))]
  pnadc_anual_visita_rendimento <- pnadc_anual_visita_rendimento[,!(names(pnadc_anual_visita_rendimento) %in% c("moradores_rendimento","rendimento_todos_trabalhos_proprioano","rendimento_todos_trabalhos_ef_proprioano","rendimento_outras_fontes_proprioano","rendimento_todos_trabalhos_ultimoano","rendimento_outras_fontes_ultimoano"))]
  pnadc_anual_visita <- merge(x=pnadc_anual_visita, y=pnadc_anual_visita_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)
  rm(pnadc_anual_visita_rendimento)
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ultimoano))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ultimoano))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ef_proprioano))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ef_proprioano))
  
  # Criando variável de faixa de rendimento domiciliar per capita em valores reais
  salariominimo_ultimoano <- 1320
  
  if (ano == 2023) {
    salariominimo_proprioano <- 1320
    
  }
  
  if (ano == 2017) {
    salariominimo_proprioano <- 937
  }
  
  if (ano == 2016) {
    salariominimo_proprioano <- 880
  }
  
  if (ano == 2018) {
    salariominimo_proprioano <- 954
  }
  if (ano == 2019) {
    salariominimo_proprioano <- 998
  }
  if (ano == 2020) {
    salariominimo_proprioano <- 1039
  }
  if (ano == 2021) {
    salariominimo_proprioano <- 1100
  }
  if (ano == 2022) {
    salariominimo_proprioano <- 1212
  }
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5009real_proprioano=as.factor(x=ifelse(VD5008real_proprioano>=0 & VD5008real_proprioano<=salariominimo_proprioano/4,"Até ¼ salário mínimo",
                                                                                               ifelse(VD5008real_proprioano>salariominimo_proprioano/4 & VD5008real_proprioano<=salariominimo_proprioano/2,"Mais de ¼ até ½ salário mínimo",
                                                                                                      ifelse(VD5008real_proprioano>salariominimo_proprioano/2 & VD5008real_proprioano<=salariominimo_proprioano,"Mais de ½ até 1 salário mínimo",
                                                                                                             ifelse(VD5008real_proprioano>salariominimo_proprioano & VD5008real_proprioano<=salariominimo_proprioano*2,"Mais de 1 até 2 salários mínimos",
                                                                                                                    ifelse(VD5008real_proprioano>salariominimo_proprioano*2 & VD5008real_proprioano<=salariominimo_proprioano*3,"Mais de 2 até 3 salários mínimos",
                                                                                                                           ifelse(VD5008real_proprioano>salariominimo_proprioano*3 & VD5008real_proprioano<=salariominimo_proprioano*5,"Mais de 3 até 5 salários mínimos",
                                                                                                                                  ifelse(VD5008real_proprioano>salariominimo_proprioano*5,"Mais de 5 salários mínimos",NA)))))))))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5009real_ultimoano=as.factor(x=ifelse(VD5008real_ultimoano>=0 & VD5008real_ultimoano<=salariominimo_ultimoano/4,"Até ¼ salário mínimo",
                                                                                              ifelse(VD5008real_ultimoano>salariominimo_ultimoano/4 & VD5008real_ultimoano<=salariominimo_ultimoano/2,"Mais de ¼ até ½ salário mínimo",
                                                                                                     ifelse(VD5008real_ultimoano>salariominimo_ultimoano/2 & VD5008real_ultimoano<=salariominimo_ultimoano,"Mais de ½ até 1 salário mínimo",
                                                                                                            ifelse(VD5008real_ultimoano>salariominimo_ultimoano & VD5008real_ultimoano<=salariominimo_ultimoano*2,"Mais de 1 até 2 salários mínimos",
                                                                                                                   ifelse(VD5008real_ultimoano>salariominimo_ultimoano*2 & VD5008real_ultimoano<=salariominimo_ultimoano*3,"Mais de 2 até 3 salários mínimos",
                                                                                                                          ifelse(VD5008real_ultimoano>salariominimo_ultimoano*3 & VD5008real_ultimoano<=salariominimo_ultimoano*5,"Mais de 3 até 5 salários mínimos",
                                                                                                                                 ifelse(VD5008real_ultimoano>salariominimo_ultimoano*5,"Mais de 5 salários mínimos",NA)))))))))
  pnadc_anual_visita$VD5009real_proprioano <- factor(x=pnadc_anual_visita$VD5009real_proprioano, levels=c("Até ¼ salário mínimo","Mais de ¼ até ½ salário mínimo","Mais de ½ até 1 salário mínimo","Mais de 1 até 2 salários mínimos","Mais de 2 até 3 salários mínimos","Mais de 3 até 5 salários mínimos","Mais de 5 salários mínimos"))
  pnadc_anual_visita$VD5009real_ultimoano <- factor(x=pnadc_anual_visita$VD5009real_ultimoano, levels=c("Até ¼ salário mínimo","Mais de ¼ até ½ salário mínimo","Mais de ½ até 1 salário mínimo","Mais de 1 até 2 salários mínimos","Mais de 2 até 3 salários mínimos","Mais de 3 até 5 salários mínimos","Mais de 5 salários mínimos"))
  
  # Realizando processo de incorporação do desenho amostral nos microdados
  pnadc_anual_visita <- tibble::as_tibble(x=pnadc_anual_visita)
  pnadc_anual_visita <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_anual_visita)
  str(object=pnadc_anual_visita)
  
  # Calculando o rendimento médio mensal real domiciliar a preços médios do ano
  #print(x=rendimento_domiciliar_media_proprioano <- survey::svybys(formula=~VD5007real_proprioano, bys=~Pais+GR+UF, design=subset(pnadc_anual_visita, V2005=="Pessoa responsável pelo domicílio"), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
  
  # Calculando o rendimento médio mensal real domiciliar per capita a preços médios do ano (SIDRA - Tabela 7531)
  #print(x=rendimento_domiciliar_per_capita_media_proprioano <- survey::svybys(formula=~VD5008real_proprioano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
  
  # Calculando o total de pessoas nas faixas de rendimento domiciliar per capita a preços médios do ano
  #print(x=faixa_rendimento_domiciliar_total_proprioano <- survey::svybys(formula=~VD5009real_proprioano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
  
  # Calculando a proporcao de pessoas nas faixas de rendimento domiciliar per capita a preços médios do ano
  #print(x=faixa_rendimento_domiciliar_proporcao_proprioano <- survey::svybys(formula=~VD5009real_proprioano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
  
  # Calculando o rendimento médio mensal real domiciliar a preços médios do último ano
  #print(x=rendimento_domiciliar_media_ultimoano <- survey::svybys(formula=~VD5007real_ultimoano, bys=~Pais+GR+UF, design=subset(pnadc_anual_visita, V2005=="Pessoa responsável pelo domicílio"), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
  
  # Calculando o rendimento médio mensal real domiciliar per capita a preços médios do último ano (SIDRA - Tabela 7533)
  #print(x=rendimento_domiciliar_per_capita_media_ultimoano <- survey::svybys(formula=~VD5008real_ultimoano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
  
  # Calculando o total de pessoas nas faixas de rendimento domiciliar per capita a preços médios do ultimo ano
  #print(x=faixa_rendimento_domiciliar_total_ultimoano <- survey::svybys(formula=~VD5009real_ultimoano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
  
  # Calculando a proporcao de pessoas nas faixas de rendimento domiciliar per capita a preços médios do ultimo ano
  #print(x=faixa_rendimento_domiciliar_proporcao_ultimoano <- survey::svybys(formula=~VD5009real_ultimoano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
  
  ####################### Minha Parte #############################
  #rendimento_domiciliar_per_capita_media_proprioano <- survey::svybys(formula=~VD5008real_proprioano, bys=~ID_DOMICILIO, design=pnadc_anual_visita, FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE)
  
  # Transformando o resultado em um data frame
  #rendimento_df <- as.data.frame(rendimento_domiciliar_per_capita_media_proprioano)
  
  # Renomeando a coluna com o valor da média para um nome mais claro
  #names(rendimento_df)[names(rendimento_df) == "VD5008real_proprioano"] <- "rendimento_domiciliar_per_capita_media_proprioano"
  
  
  # Subset of Pé-De-Meia beneficiaries (14 to 24 years,Goes to School,Public School,High School,HIpc less than half minimum wage and not null OR Received some social cash transfer,Not an single-person household )
  pnad_24_01_PdM <- subset(pnadc_anual_visita, (VD2006 == "14 a 19 anos" | VD2006 == "20 a 24 anos")
                           & V3002 == "Sim"  
                           & V3002A == "Rede pública" 
                           & V3003A == "Regular do ensino médio"  
                           & ((VD5008real_proprioano <= salariominimo_proprioano/2 & !is.na(VD5008real_proprioano)|V5001A=="Sim"|V5002A=="Sim"|V5003A=="Sim")) 
                           & VD2004 != "Unipessoal")
  
  # Subset of people with 14 to 24 years that receives some cash transfer
  pnad_yk <- subset(pnadc_anual_visita, (VD2006 == "14 a 19 anos" | VD2006 == "20 a 24 anos")
                    & ((VD5008real_proprioano <= salariominimo_proprioano/2 & !is.na(VD5008real_proprioano)|V5001A=="Sim"|V5002A=="Sim"|V5003A=="Sim")) 
                    & VD2004 != "Unipessoal")
  # Variable for Counting Observations
  pnad_24_01_PdM <- transform(pnad_24_01_PdM, contagem=1)
  pnad_yk <- transform(pnad_yk, contagem=1)
  # Population of Beneficiaries Estimation
  #print(x=contagem_beneficiados <- survey::svybys(formula=~contagem, bys=~Pais+GR+UF, design=pnad_24_01_PdM, FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
  
  # Nomes da variável que será criada para cada ano
  var_name1 <- paste0("contagem_", ano)
  
  # Criando a variável `contagem_beneficiados` para cada ano usando `survey::svybys`
  assign(var_name1, survey::svybys(
    formula = ~contagem,
    by = ~Pais + GR + UF,
    design = get(paste0("pnad_", "24", "_01_PdM")), # Obtenha o design do survey para cada ano
    FUN = svytotal,
    vartype = c("se", "cv"),
    keep.names = FALSE,
    na.rm = TRUE
  ))
  
  var_name2 <- paste0("quantil_renda_dom_pc", ano)
  
  assign(var_name2, svyquantile(x=~VD5008real_ef_proprioano, design=pnad_24_01_PdM, 
                                quantiles=c(0.1,0.25,0.5,0.75,0.9), ci=TRUE, na.rm=TRUE))
  
  var_name3 <- paste0("estats_hab", ano)
  
  if (ano == 2019 | ano == 2022) {
    assign(var_name3,svybys(formula=~S01001+S01002+S01003+S01004+S01005+S01006+S01007A+S01010+S01011A+S01013+S010141+S01024+S01028+S010311+S010312, 
                            by = ~Pais + GR + UF, 
                            design = pnad_24_01_PdM, 
                            FUN = svymean, 
                            vartype = c("se","cv"),
                            keep.names = FALSE,
                            na.rm = TRUE))
  }
  
  if (ano == 2017 | ano == 2018) {
    assign(var_name3,svybys(formula=~S01001+S01002+S01003+S01004+S01005+S01006+S01010+S01011A+S01013+S010141+S01024+S01028+S010311+S010312, 
                            by = ~Pais + GR + UF, 
                            design = pnad_24_01_PdM, 
                            FUN = svymean, 
                            vartype = c("se","cv"),
                            keep.names = FALSE,
                            na.rm = TRUE))
  }
  
  if (ano == 2016) {
    assign(var_name3,svybys(formula=~S01001+S01002+S01003+S01004+S01005+S01006+S01010+S01013+S010141+S01024+S01028+S010311+S010312, 
                            by = ~Pais + GR + UF, 
                            design = pnad_24_01_PdM, 
                            FUN = svymean, 
                            vartype = c("se","cv"),
                            keep.names = FALSE,
                            na.rm = TRUE))
  }
  #variaveis <- c("V2007", "V2010", "VD4014","VD2006","VD5008real_ef_proprioano")
  
  var_name4 <- paste0("estats_", ano)
  
  # Calcula a proporção para cada variável usando svyby
  assign(var_name4, svybys(formula = ~V2007+V2010+VD4014+VD2006+VD5008real_ef_proprioano+VD4009, 
                           by = ~Pais + GR + UF, 
                           design = pnad_24_01_PdM, 
                           FUN= svymean, 
                           vartype = c("se","cv"),
                           keep.names = FALSE,
                           na.rm = TRUE))
  
  # Adiciona o nome da variável e do ano ao resultado
  #estatisticas[[var]] <- estatisticas[[var]] %>%
  #mutate(variavel = var, ano = ano)
}

# Combina todas as estatísticas de um ano em um único data frame
#resultados_por_ano[[as.character(ano)]] <- bind_rows(estatisticas)    
#resultados_por_ano_hab[[as.character(ano)]] <- bind_rows(estatisticas_hab)
# HIpc of Beneficiaries Estimation
#print(x=rendimento_domiciliar_per_capita_media_proprioano <- survey::svybys(formula=~VD5008real_proprioano, bys=~Pais+GR+UF, design=pnad_24_01_PdM, FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

numeros <- c(1,2,3)

for (ano in anos) {
  for (x in numeros) {
    var_name5 <- paste0("contagem_",ano)
    var_name5A <- paste0(var_name5,"_",x)
    var_name6 <- paste0("estats_",ano)
    var_name6A <- paste0(var_name6,"_",x)
    var_name7 <- paste0("estats_hab",ano)
    var_name7A <- paste0(var_name7,"_",x)
    
    assign(var_name5A, as.data.frame(get(var_name5)[[x]]))
    assign(var_name6A, as.data.frame(get(var_name6)[[x]]))
    if (ano != 2023) {
      assign(var_name7A, as.data.frame(get(var_name7)[[x]]))
    }
    
  }
}

contagem_1 <- bind_rows(contagem_2016_1,contagem_2017_1,contagem_2018_1, contagem_2019_1, contagem_2022_1,contagem_2023_1)
contagem_2 <- bind_rows(contagem_2016_2,contagem_2017_2,contagem_2018_2, contagem_2019_2, contagem_2022_2,contagem_2023_2)
contagem <- bind_rows(contagem_1,contagem_2)
estats_1 <- bind_rows(estats_2016_1,estats_2017_1,estats_2018_1, estats_2019_1, estats_2022_1,estats_2023_1)
estats_2 <- bind_rows(estats_2016_2,estats_2017_2,estats_2018_2, estats_2019_2, estats_2022_2,estats_2023_2)
estats <- bind_rows(estats_1,estats_2)
estats_hab_1 <- bind_rows(estats_hab2016_1,estats_hab2017_1,estats_hab2018_1,estats_hab2019_1,estats_hab2022_1)
estats_hab_2 <- bind_rows(estats_hab2016_2,estats_hab2017_2,estats_hab2018_2,estats_hab2019_2,estats_hab2022_2)
estats_hab <- bind_rows(estats_hab_1,estats_hab_2)

write.csv(contagem,"C:/Users/rosan/OneDrive/Área de Trabalho/Eco II/Desafio/contagem.csv",row.names = FALSE)
write.csv(estats,"C:/Users/rosan/OneDrive/Área de Trabalho/Eco II/Desafio/estats.csv",row.names = FALSE)
write.csv(estats_hab,"C:/Users/rosan/OneDrive/Área de Trabalho/Eco II/Desafio/estats_hab.csv",row.names = FALSE)


# Combina todos os anos em uma única tabela
#tabela_final <- bind_rows(resultados_por_ano)