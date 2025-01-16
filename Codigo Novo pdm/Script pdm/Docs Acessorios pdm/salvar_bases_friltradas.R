## Salvar Bases de dados filtradas

# Tempo médio de carregamento das bases: 
#   - 0_CONFIG (publico_alvo_filtrado): ~15 minutos
#   - 1_CONFIG (base_evasao_filtrada): ~25 minutos
#   - 2_CONFIG (base_abandono_filtrada): ~45 minutos

# Salvar bases de dados filtradas para uso posterior

## Carregar pacotes
library(arrow)

### 0_CONFIG (publico_alvo_filtrado) ####
write_feather(publico_alvo_filtrado, '0_publico_alvo_filtrado_2015-2023.feather')

### 1_CONFIG (base_evasao_filtrada) ####
write_feather(base_evasao_filtrada, '1_base_evasao_filtrada_2015-2023.feather')

### 2_CONFIG (base_abandono_filtrada) ####
write_feather(base_abandono_filtrada, '2_base_abandono_filtrada_2015-2023.feather')

write_feather(base_abandono_pdm, '2_base_abandono_pdm_2015-2023.feather')
