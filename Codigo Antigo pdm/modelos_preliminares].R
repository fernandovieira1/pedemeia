
plot_effect <- data.frame(RDPC = seq(min(base_evasao_filtrada$RDPC, na.rm = TRUE), 
                                     max(base_evasao_filtrada$RDPC, na.rm = TRUE), length.out = 100))
plot_effect$pred <- predict(probit_splines, newdata = plot_effect, type = "response")
ggplot(plot_effect, aes(x = RDPC, y = pred)) + 
  geom_line() + 
  labs(title = "Efeito de RDPC sobre a probabilidade de evasão", 
       x = "Renda Domiciliar Per Capita", 
       y = "Probabilidade de Evasão")

library(margins)
margins_regiao <- margins(probit_regiao)
plot(margins_regiao)


margins_regiao <- margins(probit_regiao)
plot(margins_regiao)

margins_inicial <- margins(probit_inicial)
plot(margins_inicial)
summary(margins_inicial)

## Heckit
# install.packages("sampleSelection")
library(sampleSelection)

# Fórmulas para o modelo de seleção e resultado
formula_selecao <- dummylfp  ~ VD4020 + ensino_medio + educacao_mae + V2010
formula_resultado <- log_RDPC ~ ensino_medio + educacao_mae + V2010

# Modelo Heckit
base_evasao_filtrada$log_RDPC <- log(base_evasao_filtrada$RDPC + 1)


modelo_heckit <- selection(
  selection = formula_selecao, # Modelo de seleção
  outcome = formula_resultado, # Modelo de resultado
  data = base_evasao_filtrada, # Base de dados
  method = "ml"                # Estimação por máxima verossimilhança
)

# Fórmulas para o modelo de seleção e resultado
formula_selecao <- evasao ~ log_RDPC + ensino_medio + educacao_mae + V2010
formula_resultado <- log_RDPC ~ ensino_medio + educacao_mae + V2010

# Modelo Heckit
modelo_heckit <- selection(
  selection = formula_selecao,  # Modelo de seleção
  outcome = formula_resultado, # Modelo de resultado
  data = base_evasao_filtrada, # Base de dados
  method = "ml"                # Estimação por máxima verossimilhança
)

# Resumo do modelo Heckit
summary(modelo_heckit)


library(car)
vif(lm(RDPC ~ ensino_medio + educacao_mae + V2010, data = base_evasao_filtrada))

summary(base_evasao_filtrada$RDPC)
summary(base_evasao_filtrada$educacao_mae)



## Heckit 2 estágios
modelo_heckit_2step <- selection(
  selection = formula_selecao,
  outcome = formula_resultado,
  data = base_evasao_filtrada,
  method = "2step"
)
summary(modelo_heckit_2step)

