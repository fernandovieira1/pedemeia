### GRÁFICOS E TABELAS (COM PESOS)
# - Impressão do código tex.
gc(); cat('\014');

## 1.2.1A Resumo Descritivo da Cor/Raça ####
cat(a_tab_resumo_cor_latex)

## 1.2.3A Gráfico com Percentuais no Topo ####
grid.arrange(grafico_percentual, grafico_agrupado, ncol = 2)
 
## 1.3.3A Gráfico com Percentuais no Topo ####
a_graf_sexo_percentual_peso

## 1.7.1A Resumo Descritivo do RDPC por Cor ####
cat(a_tab_resumo_cor_latex2)

## Estimativa 1: Público total PDM (PNADc) ####
print(populacao_pdm)

## Estimativa 2: Público total PDM (PNADc) ####
print(populacao_pdm2)

## Estimativa 3: Evadidos por Região ####
print(proporcao_evasao)


