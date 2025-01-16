library(DBI)
library(RSQLite)

# Definir o caminho para o banco de dados
banco_sql <- "C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia\\pnad_dados.db"

# Conectar ao banco de dados (cria se não existir)
con <- dbConnect(SQLite(), banco_sql)

# Diretório contendo os arquivos .rds
local <- "C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia"

# Listar arquivos .rds
arquivos_rds <- list.files(local, pattern = "\\.rds$", full.names = TRUE)

# Iterar sobre os arquivos .rds e escrever no banco de dados
for (arquivo in arquivos_rds) {
  # Nome da tabela será baseado no nome do arquivo
  nome_tabela <- gsub("\\.rds$", "", basename(arquivo))
  
  # Carregar os dados
  dados <- readRDS(arquivo)
  
  # Escrever a tabela no banco
  dbWriteTable(con, name = nome_tabela, value = dados, overwrite = TRUE)
  message(sprintf("Tabela '%s' criada com sucesso.", nome_tabela))
}

# Fechar a conexão
dbDisconnect(con)
message("Banco de dados recriado com sucesso!")
+