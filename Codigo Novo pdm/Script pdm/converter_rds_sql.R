library(DBI)
library(RSQLite)

# Diretório contendo os arquivos
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'

# Listar arquivos .rds no diretório
arquivos <- list.files(local, pattern = '\\.rds$', full.names = TRUE)

# Nome do banco SQLite
banco_sqlite <- file.path(local, "pnad_dados.db")

# Conectar ao banco SQLite
con <- dbConnect(SQLite(), banco_sqlite)

# Converter arquivos .rds para tabelas SQLite
for (arquivo in arquivos) {
  # Carregar o arquivo .rds
  message("Convertendo arquivo: ", arquivo)
  dados <- readRDS(arquivo)
  
  # Nome da tabela com base no nome do arquivo
  nome_tabela <- tools::file_path_sans_ext(basename(arquivo))
  
  # Escrever os dados na tabela
  dbWriteTable(con, nome_tabela, dados, overwrite = TRUE)
  
  # Limpar memória
  rm(dados)
  gc()
}

# Verificar tabelas criadas no banco
tabelas <- dbListTables(con)
message("Tabelas criadas no banco de dados SQLite: ", paste(tabelas, collapse = ", "))

# Desconectar do banco
dbDisconnect(con)

