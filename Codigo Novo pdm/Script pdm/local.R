### Local de trabalho ####
local <- local
# banco_sql <- file.path(local, "pnad_dados.db")
banco_sql <- paste0(local, "\\pnad_dados.db")



#### Funções de Carregamento ####

# Carregar dados em formato RDS
carregar_dados_rds <- function(local, anos, trimestres) {
  arquivos_disponiveis <- list.files(local, pattern = 'dados_pnad_.*\\.rds$', full.names = TRUE)
  arquivos_filtrados <- arquivos_disponiveis[sapply(arquivos_disponiveis, function(x) {
    anos_arquivo <- as.numeric(unlist(regmatches(x, gregexpr('\\d{4}', x))))
    any(anos %in% anos_arquivo)
  })]
  
  lista_dados <- list()
  for (arquivo in arquivos_filtrados) {
    message('Carregando arquivo RDS: ', arquivo)
    dados <- tryCatch({ readRDS(arquivo) }, error = function(e) {
      warning(paste("Erro ao carregar o arquivo:", arquivo))
      NULL
    })
    if (!is.null(dados)) {
      dados <- dados %>% filter(Ano %in% anos, Trimestre %in% trimestres)
      lista_dados[[arquivo]] <- dados
      rm(dados); gc(full = TRUE)
    }
  }
  bind_rows(lista_dados)
}

# Carregar dados em formato SQL
carregar_dados_sql <- function(banco_sql, anos, trimestres) {
  con <- DBI::dbConnect(RSQLite::SQLite(), banco_sql)
  query <- sprintf(
    "SELECT * FROM pnad_dados WHERE Ano IN (%s) AND Trimestre IN (%s)",
    paste(anos, collapse = ","),
    paste(trimestres, collapse = ",")
  )
  dados <- tryCatch({ DBI::dbGetQuery(con, query) }, error = function(e) {
    warning("Erro ao carregar os dados do banco SQL.")
    NULL
  })
  DBI::dbDisconnect(con)
  dados
}

#### Carregar os dados ####
dados_pnad <- NULL
if (formato_arquivo == 'rds') {
  dados_pnad <- carregar_dados_rds(local, anos, trimestres)
} else if (formato_arquivo == 'sql') {
  dados_pnad <- carregar_dados_sql(banco_sql, anos, trimestres)
} else {
  stop("Formato de arquivo inválido. Escolha 'rds' ou 'sql'.")
}

### Verificar dados carregados ####
if (!is.null(dados_pnad)) {
  glimpse(dados_pnad)
} else {
  warning("Nenhum dado foi carregado.")
}