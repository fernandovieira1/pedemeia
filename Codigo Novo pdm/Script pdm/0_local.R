### Local de trabalho ####

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
  if (!file.exists(banco_sql)) {
    stop("Banco de dados SQL não encontrado: ", banco_sql)
  }
  
  # Estabelece a conexão
  con <- tryCatch({
    DBI::dbConnect(RSQLite::SQLite(), banco_sql)
  }, error = function(e) {
    stop("Erro ao conectar ao banco de dados: ", e$message)
  })
  
  # Garante que a conexão será fechada
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Lista as tabelas disponíveis
  tabelas <- DBI::dbListTables(con)
  
  # Inicializa lista para armazenar os dados
  lista_dados <- list()
  
  # Itera sobre as tabelas
  for (tabela in tabelas) {
    message(sprintf("Tentando carregar dados da tabela: %s", tabela))
    
    # Verifica se a tabela contém as colunas necessárias
    colunas <- DBI::dbListFields(con, tabela)
    if (!("Ano" %in% colunas) || !("Trimestre" %in% colunas)) {
      warning(sprintf("Tabela '%s' não contém as colunas necessárias ('Ano' e 'Trimestre').", tabela))
      next
    }
    
    # Adiciona aspas duplas ao nome da tabela
    tabela_sql <- sprintf('"%s"', tabela)
    
    # Define a query para cada tabela
    query <- sprintf(
      "SELECT * FROM %s WHERE Ano IN (%s) AND Trimestre IN (%s)",
      tabela_sql,
      paste(anos, collapse = ","),
      paste(trimestres, collapse = ",")
    )
    
    # Tenta executar a query e armazenar os resultados
    dados <- tryCatch({
      DBI::dbGetQuery(con, query)
    }, error = function(e) {
      warning(sprintf("Erro ao carregar dados da tabela '%s': %s", tabela, e$message))
      NULL
    })
    
    # Adiciona os dados à lista se não for nulo
    if (!is.null(dados) && nrow(dados) > 0) {
      lista_dados[[tabela]] <- dados
    } else {
      warning(sprintf("Tabela '%s' não retornou resultados.", tabela))
    }
  }
  
  # Combina os dados de todas as tabelas
  if (length(lista_dados) > 0) {
    return(dplyr::bind_rows(lista_dados))
  } else {
    stop("Nenhum dado foi carregado das tabelas disponíveis.")
  }
}
