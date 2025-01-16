### DADOS PNAD -- AED
# - Análise Descritiva dos dados de evasão escolar.
# - Dados empilhados e longitudinais, 
# - Análise prelimar que pode indicar variáveis a serem testadas 
# nos modelos probit, logit e heckit.
# - Apenas gráficos e tabelas salvos na AED (1_EVASAO) que serão utilizadas
# no texto do trabalho.

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
# Limpar o ambiente
#### 1.1 FAIXA ETÁRIA ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.1.3A Gráfico Inicial: Distribuição Etária por Evasão** ####
a_graf_idades_evasao

## 1.1.4A Gráfico Filtrado: Idades Válidas (14-24 Anos)** ####
a_graf_evasao_1424

#### ////// (B) DADOS LONGITUDINAIS ////// ####
## 1.1.1B Resumo Descritivo da Idade ####
htmltools::html_print(b_tab_resumo_idades)

## 1.1.3B Gráfico Inicial: Distribuição Etária por Evasão** ####
b_graf_idades_evasao

## 1.1.4B Gráfico Filtrado: Idades Válidas (14-24 Anos)** ####
b_graf_evasao_1424

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
# Limpar o ambiente

#### 1.2 COR ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.2.1A Resumo Descritivo da Cor/Raça ####
htmltools::html_print(a_tab_resumo_cor)

## 1.2.3A Gráfico com Percentuais no Topo ####
a_graf_cor_percentual

#### ////// (B) DADOS LONGITUDINAIS ////// ####
## 1.2.1B Resumo Descritivo da Cor/Raça ####
b_tab_cor_raca

## 1.2.3B Gráfico com Percentuais no Topo ####
b_graf_cor_percentual

## 1.2.4B Gráfico com Percentuais no Topo (Sem NAs)***####
b_graf_cor_percentual_sem_na

## 1.2.5B Exportação Final da Tabela (Sem NAs)** ####
htmltools::html_print(b_tab_cor_percentual_sem_na)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## 1.3 SEXO ####

#### ////// (A) DADOS EMPILHADOS ////// ####
# 1.3.1A Resumo Descritivo de Sexo ####
htmltools::html_print(a_tab_resumo_sexo)

## 1.3.3A Gráfico com Percentuais no Topo ####
a_graf_sexo_percentual

## 1.3.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
a_graf_cor_percentual_sem_na

## 1.3.5A Exportação Final da Tabela (Sem NAs em Evasao)** ####
htmltools::html_print(a_tab_resumo_sexo_sem_na)

#### ////// (B) DADOS LONGITUDINAIS ////// ####

## 1.3.1B Resumo Descritivo de Sexo ####
htmltools::html_print(b_tab_resumo_sexo)

## 1.3.3B Gráfico com Percentuais no Topo ####
b_graf_sexo_percentual

## 1.3.4B Gráfico com Percentuais no Topo (Sem NAs)** ####
b_graf_sexo_percentual_sem_na

## 1.3.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####
b_tab_sexo_percentual_sem_na

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.4 REGIÃO ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.4.1A Resumo Descritivo da Região ####
htmltools::html_print(a_tab_resumo_regiao)

## 1.4.3A Gráfico com Percentuais no Topo ####
a_graf_regiao_percentual

## 1.4.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
a_graf_regiao_percentual_sem_na

## 1.4.5A Exportação Final da Tabela (Sem NAs em Evasao)** ####
htmltools::html_print(a_tab_resumo_regiao_sem_na)

#### ////// (B) DADOS LONGITUDINAIS ////// ####

## 1.4.1B Resumo Descritivo da Região ####
htmltools::html_print(b_tab_resumo_regiao)

## 1.4.3B Gráfico com Percentuais no Topo ####
b_graf_regiao_percentual

## 1.4.4B Gráfico com Percentuais no Topo (Sem NAs)** ####
b_graf_regiao_percentual_sem_na

## 1.4.5B Exportação Final da Tabela (Sem NAs em Evasao)** ####
htmltools::html_print(b_tab_resumo_regiao_sem_na)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.5 RDPC ####

#### ////// (A) DADOS EMPILHADOS ////// ####


#### ////// (B) DADOS LONGITUDINAIS ////// ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.6 RDPC POR REGIÃO ####

#### ////// (A) DADOS EMPILHADOS ////// ####

#### ////// (B) DADOS LONGITUDINAIS ////// ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.7 RDPC POR COR ####

#### ////// (A) DADOS EMPILHADOS ////// ####

#### ////// (B) DADOS LONGITUDINAIS ////// ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.8 RDPC POR SEXO** ####

#### ////// (A) DADOS EMPILHADOS ////// ####

#### ////// (B) DADOS LONGITUDINAIS ////// ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.9 RDPC POR ENSINO MÉDIO ####

#### ////// (A) DADOS EMPILHADOS ////// ####

#### ////// (B) DADOS LONGITUDINAIS ////// ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.10 RDPC POR EVASÃO ####

#### ////// (A) DADOS EMPILHADOS ////// ####

#### ////// (B) DADOS LONGITUDINAIS ////// ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.11 RDPC POR EVASÃO E ENSINO MÉDIO** ####

#### ////// (A) DADOS EMPILHADOS ////// ####

#### ////// (B) DADOS LONGITUDINAIS ////// ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.12 RESUMO EVASÃO ####

#### ////// (A) DADOS EMPILHADOS ////// ####

#### ////// (B) DADOS LONGITUDINAIS ////// ####
