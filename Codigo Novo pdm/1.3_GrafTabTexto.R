### GRÁFICOS E TABELAS (COM PESOS)
# - Impressão do código tex.
gc(); cat('\014');

## 1.2.1A Resumo Descritivo da Cor/Raça ####
cat(a_tab_resumo_cor_latex)

## 1.2.3A Gráfico com Percentuais no Topo ####
grid.arrange(grafico_percentual, grafico_agrupado, ncol = 2)
