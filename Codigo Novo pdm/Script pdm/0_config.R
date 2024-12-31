######################## 0. CONFIGURAR AMBIENTE ########################
# Notas metodológicas
# Carregar pacotes
# Definir colunas pnad
# Criar função pnad
# Importar dados
# setwd(local)
# rm(list=ls(all=TRUE)); gc(); cat('\014')

###  0. 1 Notas metodológicas ####

## *Dicionários PNADc #####
# Descrição: Dicionários de variáveis PNADc
# Arquivos na pasta 'Docs Acessórios pdm', ou links no arquivo '1.2_Colunas_Evasao.R'.

# Complementado em '1.3_Notas_Metodologicas.R'


## *Evasão e abandono ####
# EVASAO: 1 e 5 tri - merge sexo da amostra que ingressou no 1 tri
# ABANDONO: 1 e 2 tri, 2 e 3, 3 e 4, 4 e 5 - merge sexo

### 0.2 Definir anos e trimestres ####
ano_inicial <- min(anos)
n_anos <- length(unique(anos))

### 0.3 Dados PNADc ####
# AVISO 1: Verifique o nome do arquivo e altere-o, se for o caso
# AVISO 2: Os dados da PNAD entre 2015 e 2024 encontram-se disponíveis para download em: '1.4_Notas_Metodologicas.R' >>> '0.5 Dados PNADc'

if (tipo_analise == 'censo') {
  ### Carregar múltiplos arquivos com base nos anos selecionados
  carregar_dados_pnad <- function(local, anos) {
    # Listar arquivos disponíveis no diretório
    arquivos_disponiveis <- list.files(local, pattern = 'dados_pnad_.*\\.rds$', full.names = TRUE)
    
    # Filtrar arquivos com base nos anos desejados
    arquivos_filtrados <- arquivos_disponiveis[
      sapply(arquivos_disponiveis, function(x) {
        # Extrair anos do nome do arquivo
        anos_arquivo <- as.numeric(unlist(regmatches(x, gregexpr('\\d{4}', x))))
        any(anos %in% anos_arquivo)
      })
    ]
    
    # Carregar e combinar os arquivos filtrados
    dados_combinados <- arquivos_filtrados %>%
      map_dfr(~ readRDS(.))  # Combina os dados em um único data.frame
    
    return(dados_combinados)
  }
  
  ### Carregar os dados com base nos períodos selecionados
  dados_pnad <- tryCatch({
    carregar_dados_pnad(local, anos)
  }, error = function(e) {
    warning('Erro ao carregar os arquivos de dados.')
    NULL
  })
} else {
  ### Carregar múltiplos arquivos com base nos anos selecionados
  carregar_dados_pnad <- function(local, anos, n_linhas = NULL) {
    # Listar arquivos disponíveis no diretório
    arquivos_disponiveis <- list.files(local, pattern = 'dados_pnad_.*\\.rds$', full.names = TRUE)
    
    # Filtrar arquivos com base nos anos desejados
    arquivos_filtrados <- arquivos_disponiveis[
      sapply(arquivos_disponiveis, function(x) {
        # Extrair anos do nome do arquivo
        anos_arquivo <- as.numeric(unlist(regmatches(x, gregexpr('\\d{4}', x))))
        any(anos %in% anos_arquivo)
      })
    ]
    
    # Carregar e combinar os arquivos filtrados
    dados_combinados <- arquivos_filtrados %>%
      map_dfr(function(arquivo) {
        dados <- readRDS(arquivo) # Ler o arquivo
        if (!is.null(n_linhas)) {
          dados <- dados %>% slice_sample(n = min(n_linhas, nrow(dados))) # Amostra de n linhas
        }
        return(dados)
      })
    
    return(dados_combinados)
  }
  
  ### Carregar os dados com base nos períodos selecionados
  dados_pnad <- tryCatch({
    carregar_dados_pnad(local, anos, n_linhas = 10000) # Defina n_linhas para limitar ou NULL para carregar tudo
  }, error = function(e) {
    warning('Erro ao carregar os arquivos de dados.')
    NULL
  })
}


# nrow(dados_pnad)
# ncol(dados_pnad)
# table(dados_pnad$Ano)
# table(dados_pnad$Trimestre)

### 0.4 Variáveis interesse ####

## Verificar a presença das variáveis de transferências sociais
# variaveis_transferencias <- intersect(c('VD4047', 'VDI4047'), names(dados_pnad))

## Criar ou ajustar a variável de transferências sociais
# if (length(variaveis_transferencias) > 0) {
#   # Combinar as variáveis disponíveis, se existirem
#   dados_pnad <- dados_pnad %>%
#     mutate(transferencias_sociais = rowSums(select(., all_of(variaveis_transferencias)), na.rm = TRUE))
# } else {
#   # Adicionar uma coluna de transferências sociais com NA caso não existam as variáveis
#   dados_pnad <- dados_pnad %>%
#     mutate(transferencias_sociais = NA_real_)
#   warning('Nenhuma variável de transferências sociais encontrada. Adicionando coluna com valores NA.')
# }

## Verificar estrutura do conjunto de dados atualizado
# glimpse(dados_pnad)

## Variáveis de interesse
variaveis_interesse <- c(
  
  ## IDENTIFICAÇÃO
  'ID_DOMICILIO',  # Identificador único do domicílio (não aparece dicionário)
  'UPA',           # Unidade Primária de Amostragem (UPA)
  'V1008',         # Nr. de seleção do domicílio (1 a 14)
  'V1014',         # Painel (indicador de panel)
  'V1016',         # Indica a entrevista (1 a 5)
  'Ano',           # Ano
  'Trimestre',     # Trimestre
  
  # DOMICÍLIO
  'VD2004',   # Espécie da unidade doméstica (1: unipessoal, 2: nuclear, 3: estendida, 4: composta)
  'VD2002',   # Condição/Parentesco no domicílio (responsável, cônjuge, filho, etc.)
  'V1022',    # Rural ou Urbana
  
  # FAMÍLIA
  'V2001',   # Tamanho da família (nr. de pessoas no domicílio)
  'V2003',   # Ordem do morador na família
  'V2007',   # Sexo do morador
  'V2008',   # Dia de nascimento do morador
  'V20081',  # Mês de nascimento do morador
  'V20082',  # Ano de nascimento do morador
  'V2009',   # Idade do morador
  'V2010',   # Cor ou raça do morador
  'VD2003',  # Nr. de componentes/moradores
  
  # EDUCAÇÃO
  'V3002',   # Frequenta escola?
  'V3002A',  # Tipo de escola (pública, privada, etc.)
  'V3003A',  # Curso atual ou série frequentada
  'V3006',   # Ano ou série que frequentava anteriormente
  'V3009A',  # Maior escolaridade atual do morador
  'VD3005',  # Anos de estudo completos do morador
  'V3006',   # Qual é o ano/série/semestre que frequenta?
  'V3008',   # Anteriormente frequentou escola?
  'V3013',   # Qual foi o último ano/série que concluiu com aprovação no curso frequentado anteriormente
  'V3014',   # Concluiu este curso que frequentou anteriormente?
  
  # TRABALHO E RENDA
  'V4001',   # Trabalhou na semana de referência?
  'VD4013',  # Faixa das horas habitualmente trabalhadas por semana em todos os trabalhos (14 anos ou mais)
  'VD4014',  # Faixa das horas efetivamente trabalhadas por semana em todos os trabalhos  (14 anos ou mais)
  'VD4016',  # Rendimento mensal HABITUAL do trabalho principal (FIXO) (R$)
  'VD4017',  # Rendimento mensal EFETIVO do trabalho principal (FIXO + BICOS) (R$)
  'VD4019',  # Rendimento mensal habitual (R$) de TODOS os trabalhos (apenas 1º trimestre)
  'VD4020',  # Rendimento mensal todos os trabalhos
  'V4012',   # Neste trabalho, era... (excluir militares e funcionários públicos da análise de salários)
  'V4025',   # Nesse trabalho, ... era contratado(a) como empregado temporário ?
  'V403312', # Qual era o rendimento bruto/retirada mensal que ... recebia/fazia normalmente nesse trabalho ?
  'V4039C',  # Quantas horas ... trabalhou efetivamente na semana de referência nesse trabalho principal?
  'V4040',   # Até o dia (último dia da semana de referência) fazia quanto tempo que estava nesse trabalho?
  'V405012', # Valor em dinheiro do rendimento mensal que recebia normalmente nesse trabalho secundário
  'V4056',   # Quantas horas ... trabalhava normalmente, por semana, nesse trabalho secundário?
  'VD4018',  # Tipo de remuneração recebida em todos os trabalhos (1 = dinheiro, 2 = beneficios ou sem rem.)
  'VD4019'  # Rendimento de todos os trabalhos (confirmar se é isso mesmo)
  # 'transferencias_sociais'
)

## *dados_pnad - Variáveis de interesse ####
dados_pnad <- dados_pnad %>%
  select(all_of(variaveis_interesse))

## *dados_pnad (DF) - Filtrar períodos ####
# Filtrar ano(s) e trimestre(s) de interesse
# Verificar se deu certo
if (!is.null(dados_pnad)) {
  dados_pnad <- as.data.frame(dados_pnad)
  
  dados_pnad <- dados_pnad %>%
    filter(Ano %in% anos & Trimestre %in% trimestres)
  
} else {
  warning('Os dados não foram carregados corretamente.')
}

# nrow(dados_pnad)
# table(dados_pnad$Ano)
# table(dados_pnad$Trimestre)

## *publico_alvo_filtrado (DF) #### 
# Filtrar as variáveis de interesse
publico_alvo_filtrado <- dados_pnad  # Apenas mudei o nome pelo código legado de outras versões.

nrow(publico_alvo_filtrado)
# table(publico_alvo_filtrado$Ano)
# table(publico_alvo_filtrado$Trimestre)

## Remover df dados_pnad 
# liberar RAM (COMENTAR OU DESCOMENTAR ABAIXO)
rm(dados_pnad)