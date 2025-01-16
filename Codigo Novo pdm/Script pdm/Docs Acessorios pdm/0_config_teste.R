### Local de trabalho ####

# Define o caminho para o banco SQL
banco_sql <- paste0(local, "\\pnad_dados.db")

#### Funções de Carregamento ####

# Cria índices nas tabelas SQL para otimizar as queries
criar_indices_sql <- function(banco_sql) {
  con <- tryCatch({
    DBI::dbConnect(RSQLite::SQLite(), banco_sql)
  }, error = function(e) {
    stop("Erro ao conectar ao banco de dados: ", e$message)
  })
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tabelas <- DBI::dbListTables(con)
  for (tabela in tabelas) {
    tryCatch({
      message(sprintf("Criando índices na tabela: %s", tabela))
      DBI::dbExecute(con, sprintf("CREATE INDEX IF NOT EXISTS idx_ano_trimestre ON %s (Ano, Trimestre);", tabela))
    }, error = function(e) {
      warning(sprintf("Erro ao criar índice na tabela '%s': %s", tabela, e$message))
    })
  }
  message("Índices criados com sucesso!")
}

# Carregar dados em formato SQL com paralelização
carregar_dados_sql_paralelo <- function(banco_sql, anos, trimestres) {
  if (!file.exists(banco_sql)) {
    stop("Banco de dados SQL não encontrado: ", banco_sql)
  }
  
  # Estabelece a conexão para listar tabelas
  con <- tryCatch({
    DBI::dbConnect(RSQLite::SQLite(), banco_sql)
  }, error = function(e) {
    stop("Erro ao conectar ao banco de dados: ", e$message)
  })
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Lista e filtra tabelas pelos anos especificados
  tabelas <- DBI::dbListTables(con)
  tabelas_filtradas <- tabelas[sapply(tabelas, function(t) {
    anos_tabela <- as.numeric(unlist(regmatches(t, gregexpr('\\d{4}', t))))
    any(anos %in% anos_tabela)
  })]
  
  # Paraleliza o carregamento de tabelas
  plan(multisession)  # Habilita processamento paralelo
  lista_dados <- future_map(tabelas_filtradas, function(tabela) {
    message(sprintf("Tentando carregar dados da tabela: %s", tabela))
    colunas <- DBI::dbListFields(con, tabela)
    
    if (!("Ano" %in% colunas) || !("Trimestre" %in% colunas)) {
      warning(sprintf("Tabela '%s' não contém as colunas necessárias ('Ano' e 'Trimestre').", tabela))
      return(NULL)
    }
    
    tabela_sql <- sprintf('"%s"', tabela)
    query <- sprintf(
      "SELECT * FROM %s WHERE Ano IN (%s) AND Trimestre IN (%s)",
      tabela_sql,
      paste(anos, collapse = ","),
      paste(trimestres, collapse = ",")
    )
    
    dados <- tryCatch({
      DBI::dbGetQuery(con, query)
    }, error = function(e) {
      warning(sprintf("Erro ao carregar dados da tabela '%s': %s", tabela, e$message))
      NULL
    })
    
    return(dados)
  })
  
  # Combina os resultados
  lista_dados <- compact(lista_dados)  # Remove elementos NULL
  if (length(lista_dados) > 0) {
    return(dplyr::bind_rows(lista_dados))
  } else {
    stop("Nenhum dado foi carregado das tabelas disponíveis.")
  }
}

# Executa o carregamento de acordo com o formato escolhido
if (formato_arquivo == 'rds') {
  # RDS: Carregamento direto sem otimizações SQL
  message("Carregando dados no formato RDS...")
  dados_pnad <- carregar_dados_rds(local, anos, trimestres)
  
} else if (formato_arquivo == 'sql') {
  # SQL: Executa otimizações antes de carregar
  message("Criando índices para otimizar consultas SQL...")
  criar_indices_sql(banco_sql)
  
  message("Carregando dados no formato SQL com paralelização...")
  dados_pnad <- carregar_dados_sql_paralelo(banco_sql, anos, trimestres)
  
} else {
  stop("Formato de arquivo inválido. Escolha 'rds' ou 'sql'.")
}

# Valide se o objeto foi criado
if (!exists("dados_pnad") || is.null(dados_pnad)) {
  stop("Erro: o objeto 'dados_pnad' não foi carregado corretamente.")
} else {
  message("Dados carregados com sucesso!")
}

## *publico_alvo_filtrado (DF) #### 
# Filtrar as variáveis de interesse
publico_alvo_filtrado <- dados_pnad  # Apenas mudei o nome pelo código legado de outras versões.