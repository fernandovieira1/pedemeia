### DADOS PNAD -- DESCRITIVA
# - Análise Descritiva dos dados de abandono escolar.
# - Dados empilhados e longitudinais,
# - Análise prelimar que pode indicar variáveis a serem testadas
# nos modelos probit, logit e heckit.
# - Apenas gráficos e tabelas salvos na AED (1_EVASAO) que serão utilizadas
# no texto do trabalho.
gc()

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.1 FAIXA ETÁRIA ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.1.3A Gráfico Inicial: Distribuição Etária por Evasão** ####
a_graf_ab_idades_abandono

## 1.1.4A Gráfico Filtrado: Idades Válidas (14-24 Anos)** ####
a_graf_ab_abandono_1424

#### ////// (B) DADOS LONGITUDINAIS ////// ####
## 1.1.1B Resumo Descritivo da Idade ####
htmltools::html_print(b_tab_ab_resumo_idades)

## 1.1.4B Gráfico Filtrado: Idades Válidas (14-24 Anos)** ####
b_graf_ab_abandono_1424

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.2 COR ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.2.1A Resumo Descritivo da Cor/Raça ####
htmltools::html_print(a_tab_ab_resumo_cor)

## 1.2.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
a_graf_ab_cor_percentual

#### ////// (B) DADOS LONGITUDINAIS ////// ####
## 1.2.1B Resumo Descritivo da Cor/Raça ####
htmltools::html_print(b_tab_ab_cor_raca)

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.3 SEXO ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.3.1A Resumo Descritivo de Sexo ####
htmltools::html_print(a_tab_ab_resumo_sexo)

## 1.3.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
a_graf_ab_cor_percentual_sem_na

#### ////// (B) DADOS LONGITUDINAIS ////// ####
## 1.3.1B Resumo Descritivo de Sexo ####
htmltools::html_print(b_tab_ab_resumo_sexo)

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.4 REGIÃO ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.4.1A Resumo Descritivo da Região ####
htmltools::html_print(a_tab_ab_resumo_regiao)

## 1.4.4A Gráfico com Percentuais no Topo (Sem NAs)** ####
a_graf_ab_regiao_percentual_sem_na

#### ////// (B) DADOS LONGITUDINAIS ////// ####
## 1.4.1B Resumo Descritivo da Região ####
htmltools::html_print(b_tab_ab_resumo_regiao)

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
# #### 1.5 RDPC ####
# 
# #### ////// (A) DADOS EMPILHADOS ////// ####
# ## 1.5.5A Exportação Final da Tabela (Sem NAs em abandono)** ####
# htmltools::html_print(a_tab_ab_resumo_rdpc_sem_na)
# 
# #### ////// (B) DADOS LONGITUDINAIS ////// ####
# ## 1.5.1B Resumo Descritivo do RDPC ####
# htmltools::html_print(b_tab_ab_resumo_rdpc_ano)
# 
# ## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
# #### 1.6 RDPC POR REGIÃO ####
# 
# #### ////// (A) DADOS EMPILHADOS ////// ####
# ## 1.6.5A Exportação Final da Tabela (Sem NAs em abandono)** ####
# htmltools::html_print(a_tab_ab_resumo_rdpc_regiao_sem_na)
# 
# #### ////// (B) DADOS LONGITUDINAIS ////// ####
# ## 1.6.1B Resumo Descritivo do RDPC por Região ####
# htmltools::html_print(b_tab_ab_resumo_rdpc_regiao)
# 
# ## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
# #### 1.7 RDPC POR COR ####
# 
# #### ////// (A) DADOS EMPILHADOS ////// ####
# ## 1.7.5A Exportação Final da Tabela (Sem NAs em abandono e Cor)** ####
# htmltools::html_print(a_tab_ab_resumo_rdpc_cor_sem_na)
# 
# #### ////// (B) DADOS LONGITUDINAIS ////// ####
# ## 1.7.1B Resumo Descritivo do RDPC por Cor ####
# htmltools::html_print(b_tab_ab_resumo_rdpc_cor)
# 
# ## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
# #### 1.8 RDPC POR SEXO ####
# 
# #### ////// (A) DADOS EMPILHADOS ////// ####
# ## 1.8.4A Exportação Final ####
# htmltools::html_print(a_tab_ab_resumo_rdpc_sexo_sem_na)
# 
# #### ////// (B) DADOS LONGITUDINAIS ////// ####
# ## 1.8.1B Resumo Descritivo do RDPC por Sexo ####
# htmltools::html_print(b_tab_ab_resumo_rdpc_sexo)
# 
# ## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
# #### 1.9 RDPC POR ENSINO MÉDIO ####
# 
# #### ////// (A) DADOS EMPILHADOS ////// ####
# ## 1.9.4A Exportação Final ####
# htmltools::html_print(a_tab_ab_resumo_rdpc_ensino_medio_sem_na)
# 
# #### ////// (B) DADOS LONGITUDINAIS ////// ####
# ## 1.9.1B Resumo Descritivo ####
# htmltools::html_print(b_tab_ab_resumo_rdpc_ensino_medio)
# 
# ## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
# #### 1.10 RDPC POR ABANDONO ####
# 
# #### ////// (A) DADOS EMPILHADOS ////// ####
# ## 1.10.4A Exportação Final ####
# htmltools::html_print(a_tab_ab_resumo_rdpc_abandono_sem_na)
# 
# #### ////// (B) DADOS LONGITUDINAIS ////// ####
# ## 1.10.1B Resumo Descritivo ####
# htmltools::html_print(b_tab_ab_resumo_rdpc_abandono)
# 
# ## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
# #### 1.11 RDPC POR ABANDONO E ENSINO MÉDIO ####
# 
# #### ////// (A) DADOS EMPILHADOS ////// ####
# ## 1.11.5A Exportação Final ####
# htmltools::html_print(a_tab_ab_resumo_rdpc_abandono_ensino_sem_na)
# 
# #### ////// (B) DADOS LONGITUDINAIS ////// ####
# ## 1.11.1B Resumo Descritivo ####
# htmltools::html_print(b_tab_ab_resumo_rdpc_abandono_ensino)

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
#### 1.12 POPULAÇÃO (RURAL VS URBANA) ####

#### ////// (A) DADOS EMPILHADOS ////// ####
## 1.12.5A Exportação Final da Tabela ####
htmltools::html_print(a_tab_ab_resumo_populacao_sem_na)

#### ////// (B) DADOS LONGITUDINAIS ////// ####
## 1.12.1B Resumo Descritivo da População Segmentada por Ano ####
htmltools::html_print(b_tab_ab_resumo_populacao)
    
                          