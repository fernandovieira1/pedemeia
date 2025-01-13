# Exibir os tipos originais das colunas
cat('Tipos originais das colunas:\n')
str(publico_alvo_filtrado)

# Identificar colunas string (character) e numeric
string_cols <- names(publico_alvo_filtrado)[sapply(publico_alvo_filtrado, is.character)]
numeric_cols <- names(publico_alvo_filtrado)[sapply(publico_alvo_filtrado, is.numeric)]

# Converter strings para fatores
if (length(string_cols) > 0) {
  publico_alvo_filtrado[string_cols] <- lapply(publico_alvo_filtrado[string_cols], as.factor)
  cat('\nColunas string convertidas para fatores:\n')
  print(string_cols)
} else {
  cat('\nNenhuma coluna string foi encontrada para converter.\n')
}

# Reduzir colunas numeric para integer, se possível
if (length(numeric_cols) > 0) {
  publico_alvo_filtrado[numeric_cols] <- lapply(publico_alvo_filtrado[numeric_cols], function(col) {
    if (all(col == floor(col), na.rm = TRUE)) {
      as.integer(col)  # Converte para integer se todos os valores forem inteiros
    } else {
      col  # Mantém como numeric
    }
  })
  cat('\nColunas numeric reduzidas para tipos menores (se possível):\n')
  print(numeric_cols)
} else {
  cat('\nNenhuma coluna numeric foi encontrada para reduzir.\n')
}

# Exibir os tipos finais das colunas
cat('\nTipos finais das colunas após otimização:\n')
str(publico_alvo_filtrado)
