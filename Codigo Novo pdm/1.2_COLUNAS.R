### DADOS PNAD -- Descricao das Colunas

######################## 1. BASE EVASÃO ########################
names(base_evasao_filtrada)

# [1] "id_individuo"       - Identificador único para cada indivíduo.
# [2] "Ano"                - Ano da pesquisa.
# [3] "Trimestre"          - Trimestre de referência da pesquisa.
# [4] "ensino_medio"       - Dummy indicando se o indivíduo concluiu o ensino médio (1 = sim, 0 = não).
# [5] "VD4020"             - Rendimento EFETIVO do trabalho principal (em valores monetários).
# [6] "RD"                 - Rendimento domiciliar TOTAL (soma dos rendimentos dos membros do domicílio).
# [7] "RDPC"               - Rendimento domiciliar PER CAPITA.
# [8] "RDPC_menor_meio_sm" - Indicador se o RDPC é menor que meio salário mínimo (1 = sim, 0 = não).
# [9] "regiao"             - Região de residência do entrevistado (Norte, Nordeste, Sudeste, Sul, Centro-Oeste).
# [10] "educacao_mae"       - Nível educacional da mãe do entrevistado.
# [11] "educacao_pai"       - Nível educacional do pai do entrevistado.
# [12] "evasao"             - Dummy indicando evasão escolar (1 = evasão, 0 = não evasão).
# [13] "salario_minimo"     - Valor do salário mínimo no ano/trimestre de referência.
# [14] "ID_DOMICILIO"       - Identificador único do domicílio.
# [15] "UPA"                - Unidade Primária de Amostragem.
# [16] "V1008"              - Número do setor censitário.
# [17] "V1014"              - Estrato de seleção amostral.
# [18] "V1016"              - Número da entrevista (1 a 5).
# [19] "VD2004"             - Espécie da unidade doméstica (Unipessoal, Nuclear, Estendida, Composta).
# [20] "VD2002"             - Condição no domicílio (Pai, mãe...) [Célula E903].
# [21] "V2001"              - Número de pessoas no domicílio.
# [22] "V2003"              - Número de ordem da pessoa no domicílio.
# [23] "V2007"              - Sexo.
# [24] "V2008"              - Dia de nascimento.
# [25] "V20081"             - Mês de nascimento.
# [26] "V20082"             - Ano de nascimento.
# [27] "V2009"              - Idade do morador na data de referência.
# [28] "V2010"              - Cor ou raça.
# [29] "VD2003"             - Número de componentes no domicílio (excluindo pensionistas/empregados domésticos).
# [30] "V3002"              - Frequenta escola atualmente? (1 = sim, 0 = não).
# [31] "V3002A"             - Tipo de escola (pública ou privada).
# [32] "V3003A"             - Qual é o curso que frequenta? (fundamental, médio, superior).
# [33] "V3006"              - Série/ano que está cursando atualmente (do 1º ao 12º).
# [34] "V3009A"             - Curso mais elevado que frequentou (da creche ao doutorado).
# [35] "VD3005"             - Anos de estudo.
# [36] "V3008"              - Anteriormente ... frequentou escola? (1 = sim, 0 = não).
# [37] "V3013"              - Qual foi o último ano/série/semestre que concluiu com aprovação  (do 1º ao 12º).
# [38] "V3014"              - Concluiu este curso que frequentou anteriormente? (1 = sim, 0 = não).
# [39] "VD4013"             - Faixa das horas HABITUALMENTE trabalhadas por semana em todos os trabalhos para pessoas de 14 anos ou mais de idade [célula C1039].
# [40] "VD4014"             - Faixa das horas EFETIVAMENTE trabalhadas por semana em todos os trabalhos para pessoas de 14 anos ou mais de idade [célula C1045].
# [41] "VD4016"             - Rendimento mensal HABITUAL do trabalho principal para pessoas de 14 anos ou mais de idade.
# [42] "VD4017"             - Rendimento mensal EFETIVO do trabalho principal para pessoas de 14 anos ou mais de idade.
# [43] "VD4019"             - Rendimento mensal DE TODOS os trabalhos para pessoas de 14 anos ou mais de idade..
# [44] "V4012"              - Tipo de vínculo empregatício (militar, funcionário etc.).
# [45] "V4025"              - Nesse trabalho era contratado(a) como empregado temporário?
# [46] "V403312"            - Rendimento bruto/retirada mensal.
# [47] "V4039C"             - Quantas horas ... trabalhou efetivamente na semana de referência nesse trabalho principal?
# [48] "V4040"              - Até o dia ... (último dia da semana de referência) fazia quanto tempo que ... estava nesse trabalho?
# [49] "V405012"            - Valor em dinheiro do rendimento mensal que recebia normalmente nesse trabalho secundário.
# [50] "V4056"              - Quantas horas ... trabalhava normalmente, por semana, nesse trabalho secundário?
# [51] "VD4018"             - Tipo de remuneração habitualmente recebida em todos os trabalhos para pessoas de 14 anos ou mais de idade
# [52] "is_mae"             - Dummy indicando se o indivíduo é mãe (1 = sim, 0 = não).
# [53] "is_pai"             - Dummy indicando se o indivíduo é pai (1 = sim, 0 = não).
