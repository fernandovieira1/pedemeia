### Local de trabalho ####

# Define o caminho para o banco SQL
banco_sql <- paste0(local, "\\pnad_dados.db")

#### Funções de Carregamento ####

# Carregar dados em formato SQL com filtro por anos
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
  
  # Lista as tabelas disponíveis e filtra pelas que contêm os anos
  tabelas <- DBI::dbListTables(con)
  tabelas_filtradas <- tabelas[sapply(tabelas, function(t) {
    anos_tabela <- as.numeric(unlist(regmatches(t, gregexpr('\\d{4}', t))))
    any(anos %in% anos_tabela)
  })]
  
  # Inicializa lista para armazenar os dados
  lista_dados <- list()
  
  # Itera sobre as tabelas filtradas
  for (tabela in tabelas_filtradas) {
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

# Verifique o formato e carregue os dados
if (formato_arquivo == 'rds') {
  message("Carregando dados no formato RDS...")
  dados_pnad <- carregar_dados_rds(local, anos, trimestres)
} else if (formato_arquivo == 'sql') {
  message("Carregando dados no formato SQL...")
  dados_pnad <- carregar_dados_sql(banco_sql, anos, trimestres)
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

rm(dados_pnad)