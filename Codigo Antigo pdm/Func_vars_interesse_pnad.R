# Função com as novas variáveis sugeridas pelo Daniel na aula do dia 26/11
variaveis_interesse <- c(
  
  ## IDENTIFICAÇÃO
  'ID_DOMICILIO',  # Identificador único do domicílio (não aparece dicionário)
  'UPA',          # Unidade Primária de Amostragem (UPA)
  'V1008',        # Nr. de seleção do domicílio (1 a 14)
  'V1014',        # Painel (indicador de panel)
  'V1016',        # Indica a entrevista (1 a 5)
  'Ano',          # Ano
  'Trimestre',    # Trimestre
  
  # DOMICÍLIO
  'VD2004',  # Espécie da unidade doméstica (1: unipessoal, 2: nuclear, 3: estendida, 4: composta)
  'VD2002',   # Condição/Parentesco no domicílio (responsável, cônjuge, filho, etc.)
  
  # FAMÍLIA
  'V2001',   # Tamanho da família (nr. de pessoas no domicílio)
  'V2003',   # Ordem do morador na família
  'V2007',   # Sexo do morador
  'V2008',   # Dia de nascimento do morador
  'V20081',  # Mês de nascimento do morador
  'V20082',  # Ano de nascimento do morador
  'V2009',   # Idade do morador
  'V2010',   # Cor ou raça do morador
  'VD2003',  # Nr. de componentes/moradores
  
  # EDUCAÇÃO
  'V3002',   # Frequenta escola?
  'V3002A',  # Tipo de escola (pública, privada, etc.)
  'V3003A',  # Curso atual ou série frequentada
  'V3006',   # Ano ou série que frequentava anteriormente
  'V3009A',  # Maior escolaridade atual do morador
  'VD3005',  # Anos de estudo completos do morador
  'V3006',   # Qual é o ano/série/semestre que frequenta?
  'V3008',   # Anteriormente frequentou escola?
  'V3013',   # Qual foi o último ano/série que concluiu com aprovação no curso frequentado anteriormente
  'V3014',   # Concluiu este curso que frequentou anteriormente?
  
  # TRABALHO E RENDA
  'VD4013',  # Faixa das horas habitualmente trabalhadas por semana em todos os trabalhos (14 anos ou mais)
  'VD4014',  # Faixa das horas efetivamente trabalhadas por semana em todos os trabalhos  (14 anos ou mais)
  'VD4016',  # Rendimento mensal habitual (R$)
  'VD4017',  # Rendimento mensal efetivo (R$)
  'VD4019',  # Rendimento mensal habitual (R$) (apenas 1º trimestre)
  'VD4020',  # Rendimento mensal todos os trabalhos
  'V4012',   # Neste trabalho, era... (excluir militares e funcionários públicos da análise de salários)
  'V4025',   # Nesse trabalho, ... era contratado(a) como empregado temporário ?
  'V403312', # Qual era o rendimento bruto/retirada mensal que ... recebia/fazia normalmente nesse trabalho ?
  'V4039C',  # Quantas horas ... trabalhou efetivamente na semana de referência nesse trabalho principal?
  'V4040',   # Até o dia (último dia da semana de referência) fazia quanto tempo que estava nesse trabalho?
  'V405012', # Valor em dinheiro do rendimento mensal que recebia normalmente nesse trabalho secundário
  'V4056',   # Quantas horas ... trabalhava normalmente, por semana, nesse trabalho secundário?
  'VD4018',  # Tipo de remuneração recebida em todos os trabalhos (1 = dinheiro, 2 = beneficios ou sem rem.)
  'VD4019'   # Rendimento de todos os trabalhos (confirmar se é isso mesmo)
)
