# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc
dados_sinasc = read.csv("SINASC_2015.csv", sep = ";")

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
dados_sinasc_1 = dados_sinasc[,c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 
dados_sinasc_2 = subset(dados_sinasc_1, substr(CODMUNRES, 1, 2) == "27")

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 

# Exportar o arquivo com o nome dados_sinasc_2.csv
write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"

# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
LOCNASC_f = table(dados_sinasc_2$LOCNASC)
ESTCIVMAE_f = table(dados_sinasc_2$ESTCIVMAE)
GESTACAO_f = table(dados_sinasc_2$GESTACAO)
GRAVIDEZ_f = table(dados_sinasc_2$GRAVIDEZ)
PARTO_f = table(dados_sinasc_2$PARTO)
SEXO_f = table(dados_sinasc_2$SEXO)
APGAR5_f = table(dados_sinasc_2$APGAR5)
RACACOR_f = table(dados_sinasc_2$RACACOR)
IDANOMAL_f = table(dados_sinasc_2$IDANOMAL)
ESCMAE2010_f = table(dados_sinasc_2$ESCMAE2010)
RACACORMAE_f = table(dados_sinasc_2$RACACORMAE)
TPAPRESENT_f = table(dados_sinasc_2$TPAPRESENT)
TPROBSON_f = table(dados_sinasc_2$TPROBSON)
PARIDADE_f = table(dados_sinasc_2$PARIDADE)
KOTELCHUCK_f = table(dados_sinasc_2$KOTELCHUCK)
CONSPRENAT_f = table(dados_sinasc_2$CONSPRENAT)

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADEMAE, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA
summary(dados_sinasc_2)

# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados
dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4),
                                labels = c("Hospital", "Outros estabelecimentos de saúde",
                                           "Domicílio", "Outros"))

dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5),
                                  labels = c("Solteira", "Casada", "Viúva",
                                             "Separada judicialmente/divorciada",
                                             "União estável"))

dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6),
                                 labels = c("Menos de 22 semanas", "22 a 27 semanas",
                                             "28 a 31 semanas", "32 a 36 semanas",
                                             "32 a 36 semanas", "42 semanas e mais"))

dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3),
                                 labels = c("Única", "Dupla", "Tripla ou mais"))

dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2),
                              labels = c("Vaginal", "Cesário"))

dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2),
                             labels = c("Masculino", "Feminino"))

dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5),
                                labels = c("Branca", "Preta", "Amarela",
                                           "Parda", "Indígena"))

dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2),
                                 labels = c("Sim", "Não"))

dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5),
                                   labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)",
                                              "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)",
                                              "Superior incompleto", "Superior completo"))

dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5),
                                   labels = c("Branca", "Preta", "Amarela",
                                              "Parda", "Indígena"))

dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3),
                                   labels = c("Cefálico", "Pélvica ou podálica",
                                              "Transversa"))

dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = c(1,2,3,4,5,6,7,8,9,10),
                                 labels = c("Grupo 1", "Grupo 2", "Grupo 3",
                                            "Grupo 4", "Grupo 5", "Grupo 6",
                                            "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))

dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0,1),
                                 labels = c("Nulípara", "Multípara"))

dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5),
                                   labels = c("Não realizou pré-natal", "Inadequado",
                                              "Intermediário", "Adequado", "Mais que adequado"))

# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
dados_sinasc_2$F_PESO <- cut(dados_sinasc_2$PESO,
                             breaks = c(-Inf, 2499, 3999, Inf),
                             labels = c("Baixo peso", "Peso normal", "Macrossomia"),
                             right = TRUE)

# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
dados_sinasc_2$F_IDADE <- cut(dados_sinasc_2$IDADEMAE,
                              breaks = c(-Inf, 14, 19, 24, 29, 34, 39, 44, 49, Inf),
                              labels = c("<15","15-19","20-24","25-29",
                                         "30-34","35-39","40-44","45-49","50+"),
                              right = TRUE)

# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
dados_sinasc_2$F_APGAR5 <- ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 <- as.factor(dados_sinasc_2$F_APGAR5)

# Atenção para casos de NA em IDADEMAE, PESO e APGAR5

# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
dados_sinasc_2$PERIG <- ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES,
                               "Não","Sim")
dados_sinasc_2$PERIG <- as.factor(dados_sinasc_2$PERIG)

# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
dados_sinasc_2$ESTCIV <- ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"),
                                "Sem companheiro","Com companheiro")
dados_sinasc_2$ESTCIV <- as.factor(dados_sinasc_2$ESTCIV)

# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.
tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep=";")
tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))
dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC","SEXO"), all.x = TRUE)
dados_sinasc_2$F_PIG = ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
                              ifelse(is.na(dados_sinasc_2$PESO)|is.na(dados_sinasc_2$PESO_P10)|is.na(dados_sinasc_2$PESO_P90),NA,
                                     ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                            ifelse(dados_sinasc_2$PESO<=dados_sinasc_2$PESO_P90, "AIG", "GIG"))))
dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG"))

# Tarefas 9 e 10 (reformulada)
# Crie um banco de dados contendo as 103 variáveis listadas no arquivo “Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”
# O banco final deverá possuir: 103 colunas, correspondentes às variáveis especificadas;
# n + 1 linhas, onde: n corresponde ao número de municípios distintos da UF em análise
# a primeira linha corresponde aos valores agregados para a UF como um todo;
# as demais linhas correspondem aos municípios da UF.
# As variáveis devem ser construídas a partir dos microdados do SINASC, respeitando os nomes e a ordem especificados.

# Base inicial (municípios)
# Cria um dataframe com uma única coluna (CODMUNRES) com valores ordenados e sem repetição
base = data.frame(CODMUNRES =sort(unique(dados_sinasc_2$CODMUNRES)))

# TN - total de nascimentos
TN = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES)))
names(TN) = c("CODMUNRES","TN")
base = merge(base, TN, by = "CODMUNRES", all.x = TRUE)

# TNRC - completos nas 61 variáveis
dados_UF = dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES), 1, 2) == "27",]
dados_UF_comp = dados_UF[complete.cases(dados_UF), ]
TNRC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRC) = c("CODMUNRES","TNRC")
base = merge(base, TNRC, by = "CODMUNRES", all.x = TRUE)

# TNRCR - completos nas 22 variáveis
dados_UF_1 = dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "27",]
dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1), ]
TNRCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRCR) = c("CODMUNRES","TNRCR")
base = merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)

# TGI_15 - total de gestantes com idade inferior a 15 anos
TGI_15 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE < 15],
         levels = base$CODMUNRES)))
names(TGI_15) = c("CODMUNRES","TGI_15")
base = merge(base, TGI_15, by="CODMUNRES", all.x=TRUE)

# TGI_15_19 -	total de gestantes com idade >=15 e <=19 anos
TGI_15_19 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 19],
    levels = base$CODMUNRES)))
names(TGI_15_19) = c("CODMUNRES","TGI_15_19")
base = merge(base, TGI_15_19, by="CODMUNRES", all.x=TRUE)

# TGI_20_24	- total de gestantes com idade >=20 e <=24 anos
TGI_20_24 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$IDADEMAE >= 20 & dados_sinasc_2$IDADEMAE <= 24],
    levels = base$CODMUNRES)))
names(TGI_20_24) = c("CODMUNRES","TGI_20_24")
base = merge(base, TGI_20_24, by="CODMUNRES", all.x=TRUE)

# TGI_25_29	- total de gestantes com idade >=25 e <=29 anos
TGI_25_29 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$IDADEMAE >= 25 & dados_sinasc_2$IDADEMAE <= 29],
    levels = base$CODMUNRES)))
names(TGI_25_29) = c("CODMUNRES","TGI_25_29")
base = merge(base, TGI_25_29, by="CODMUNRES", all.x=TRUE)

# TGI_30_34	- total de gestantes com idade >=30 e <=34 anos
TGI_30_34 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$IDADEMAE >= 30 & dados_sinasc_2$IDADEMAE <= 34],
    levels = base$CODMUNRES)))
names(TGI_30_34) = c("CODMUNRES","TGI_30_34")
base = merge(base, TGI_30_34, by="CODMUNRES", all.x=TRUE)

# TGI_35_39 -	total de gestantes com idade >=35 e <=39 anos
TGI_35_39 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$IDADEMAE >= 35 & dados_sinasc_2$IDADEMAE <= 39],
    levels = base$CODMUNRES)))
names(TGI_35_39) = c("CODMUNRES","TGI_35_39")
base = merge(base, TGI_35_39, by="CODMUNRES", all.x=TRUE)

# TGI_40_44 -	total de gestantes com idade >=40 e <=44 anos
TGI_40_44 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$IDADEMAE >= 40 & dados_sinasc_2$IDADEMAE <= 44],
    levels = base$CODMUNRES)))
names(TGI_40_44) = c("CODMUNRES","TGI_40_44")
base = merge(base, TGI_40_44, by="CODMUNRES", all.x=TRUE)

# TGI_45_49 -	total de gestantes com idade >=45 e <=49 anos
TGI_45_49 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$IDADEMAE >= 45 & dados_sinasc_2$IDADEMAE <= 49],
    levels = base$CODMUNRES)))
names(TGI_45_49) = c("CODMUNRES","TGI_45_49")
base = merge(base, TGI_45_49, by="CODMUNRES", all.x=TRUE)

# TGI_50 - total de gestantes com idade >=50
TGI_50 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 50],
         levels = base$CODMUNRES)))
names(TGI_50) = c("CODMUNRES","TGI_50")
base = merge(base, TGI_50, by="CODMUNRES", all.x=TRUE)

# TGIF - total de gestantes em idade fértil, ou seja, idade >=15 e <=49 anos
TGIF = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 49],
    levels = base$CODMUNRES)))
names(TGIF) = c("CODMUNRES","TGIF")
base = merge(base, TGIF, by="CODMUNRES", all.x=TRUE)

# IM_P25 - percentil 25 da idade materna
IM_P25 = aggregate(IDADEMAE ~ CODMUNRES,
                   data = dados_sinasc_2,
                   function(x) quantile(x, 0.25, na.rm = TRUE))
names(IM_P25) = c("CODMUNRES","IM_P25")
base = merge(base, IM_P25, by = "CODMUNRES", all.x = TRUE)

# IM_P50 - percentil 50 da idade materna
IM_P50 = aggregate(IDADEMAE ~ CODMUNRES,
                   data = dados_sinasc_2,
                   function(x) quantile(x, 0.5, na.rm = TRUE))
names(IM_P50) = c("CODMUNRES","IM_P50")
base = merge(base, IM_P50, by = "CODMUNRES", all.x = TRUE)

# IM_P75 - percentil 75 da idade materna
IM_P75 = aggregate(IDADEMAE ~ CODMUNRES,
                   data = dados_sinasc_2,
                   function(x) quantile(x, 0.75, na.rm = TRUE))
names(IM_P75) = c("CODMUNRES","IM_P75")
base = merge(base, IM_P75, by = "CODMUNRES", all.x = TRUE)

# IM_MD - idade média materna
IM_MD = aggregate(IDADEMAE ~ CODMUNRES, data = dados_sinasc_2, mean, na.rm = TRUE)
names(IM_MD) = c("CODMUNRES","IM_MD")
base = merge(base, IM_MD, by = "CODMUNRES", all.x = TRUE)
base$IM_MD = round(base$IM_MD, 2)

# IM_DP - desvio-padrão da idade materna
IM_DP = aggregate(IDADEMAE ~ CODMUNRES, data = dados_sinasc_2, sd, na.rm = TRUE)
names(IM_DP) = c("CODMUNRES","IM_DP")
base = merge(base, IM_DP, by = "CODMUNRES", all.x = TRUE)
base$IM_DP = round(base$IM_DP, 2)

# EM_S - total de gestantes sem escolaridade
EM_S = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Sem escolaridade"],
                                  levels = base$CODMUNRES)))
names(EM_S) = c("CODMUNRES","EM_S")
base = merge(base, EM_S, by = "CODMUNRES", all.x = TRUE)

# EM_FI - total de gestantes com escolaridade fundamental I
EM_FI = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Fundamental I (1ª a 4ª série)"],
                                   levels = base$CODMUNRES)))
names(EM_FI) = c("CODMUNRES","EM_FI")
base = merge(base, EM_FI, by = "CODMUNRES", all.x = TRUE)

# EM_FII - total de gestantes com escolaridade fundamental II
EM_FII = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Fundamental II (5ª a 8ª série)"],
                                    levels = base$CODMUNRES)))
names(EM_FII) = c("CODMUNRES","EM_FII")
base = merge(base, EM_FII, by = "CODMUNRES", all.x = TRUE)

# EM_M - total de gestantes com escolaridade médio
EM_M = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Médio (antigo 2º grau)"],
                                  levels = base$CODMUNRES)))
names(EM_M) = c("CODMUNRES","EM_M")
base = merge(base, EM_M, by = "CODMUNRES", all.x = TRUE)

# EM_SI - total de gestantes com escolaridade superior incompleto
EM_SI = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Superior incompleto"],
                                   levels = base$CODMUNRES)))
names(EM_SI) = c("CODMUNRES","EM_SI")
base = merge(base, EM_SI, by = "CODMUNRES", all.x = TRUE)

# EM_SC - total de gestantes com escolaridade superior completo
EM_SC = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Superior completo"],
                                   levels = base$CODMUNRES)))
names(EM_SC) = c("CODMUNRES","EM_SC")
base = merge(base, EM_SC, by = "CODMUNRES", all.x = TRUE)

# TGRC_B - total de gestantes da raça/cor branca
TGRC_B = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Branca"],
                                   levels = base$CODMUNRES)))
names(TGRC_B) = c("CODMUNRES","TGRC_B")
base = merge(base, TGRC_B, by = "CODMUNRES", all.x = TRUE)

# TGRC_PT - total de gestantes da raça/cor preta
TGRC_PT = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Preta"],
                                    levels = base$CODMUNRES)))
names(TGRC_PT) = c("CODMUNRES","TGRC_PT")
base = merge(base, TGRC_PT, by = "CODMUNRES", all.x = TRUE)

# TGRC_A - total de gestantes da raça/cor amarela
TGRC_A = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Amarela"],
                                     levels = base$CODMUNRES)))
names(TGRC_A) = c("CODMUNRES","TGRC_A")
base = merge(base, TGRC_A, by = "CODMUNRES", all.x = TRUE)

# TGRC_PD - total de gestantes da raça/cor parda
TGRC_PD = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Parda"],
                                    levels = base$CODMUNRES)))
names(TGRC_PD) = c("CODMUNRES","TGRC_PD")
base = merge(base, TGRC_PD, by = "CODMUNRES", all.x = TRUE)

# TGRC_I - total de gestantes da raça/cor indígena
TGRC_I = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Indígena"],
                                     levels = base$CODMUNRES)))
names(TGRC_I) = c("CODMUNRES","TGRC_I")
base = merge(base, TGRC_I, by = "CODMUNRES", all.x = TRUE)

# TGSC - total de gestantes sem companheiro
TGSC = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESTCIV == "Sem companheiro"],
                                    levels = base$CODMUNRES)))
names(TGSC) = c("CODMUNRES","TGSC")
base = merge(base, TGSC, by = "CODMUNRES", all.x = TRUE)

# TGCC - total de gestantes com companheiro
TGCC = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESTCIV == "Com companheiro"],
                                  levels = base$CODMUNRES)))
names(TGCC) = c("CODMUNRES","TGCC")
base = merge(base, TGCC, by = "CODMUNRES", all.x = TRUE)

# TGPRI - total de gestantes primíparas
TGPRI = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PARIDADE == "Nulípara"],
                                  levels = base$CODMUNRES)))
names(TGPRI) = c("CODMUNRES","TGPRI")
base = merge(base, TGPRI, by = "CODMUNRES", all.x = TRUE)

# TGNPRI - total de gestantes não primíparas
TGNPRI = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PARIDADE == "Multípara"],
                                   levels = base$CODMUNRES)))
names(TGNPRI) = c("CODMUNRES","TGNPRI")
base = merge(base, TGNPRI, by = "CODMUNRES", all.x = TRUE)

# TGU - total de gestações únicas
TGU = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GRAVIDEZ == "Única"],
                                    levels = base$CODMUNRES)))
names(TGU) = c("CODMUNRES","TGU")
base = merge(base, TGU, by = "CODMUNRES", all.x = TRUE)

# TGG - total de gestações gemelares
TGG = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GRAVIDEZ %in% c("Dupla","Tripla ou mais")],
                                 levels = base$CODMUNRES)))
names(TGG) = c("CODMUNRES","TGG")
base = merge(base, TGG, by = "CODMUNRES", all.x = TRUE)

# TGD_22 - total de gestações com duração inferior a 22 semanas
TGD_22 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$SEMAGESTAC < 22],
         levels = base$CODMUNRES)))
names(TGD_22) = c("CODMUNRES","TGD_22")
base = merge(base, TGD_22, by="CODMUNRES", all.x=TRUE)

# TGD_22_27 - total de gestações com duração >=22 e <=27
TGD_22_27 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$SEMAGESTAC >= 22 & dados_sinasc_2$SEMAGESTAC <= 27],
    levels = base$CODMUNRES)))
names(TGD_22_27) = c("CODMUNRES","TGD_22_27")
base = merge(base, TGD_22_27, by="CODMUNRES", all.x=TRUE)

# TGD_28_31 - total de gestações com duração >=28 e <=31
TGD_28_31 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$SEMAGESTAC >= 28 & dados_sinasc_2$SEMAGESTAC <= 31],
    levels = base$CODMUNRES)))
names(TGD_28_31) = c("CODMUNRES","TGD_28_31")
base = merge(base, TGD_28_31, by="CODMUNRES", all.x=TRUE)

# TGD_32_36 - total de gestações com duração >=32 e <=36
TGD_32_36 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$SEMAGESTAC >= 32 & dados_sinasc_2$SEMAGESTAC <= 36],
    levels = base$CODMUNRES)))
names(TGD_32_36) = c("CODMUNRES","TGD_32_36")
base = merge(base, TGD_32_36, by="CODMUNRES", all.x=TRUE)

# TGD_37_41 - total de gestações com duração >=37 e <=41
TGD_37_41 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41],
    levels = base$CODMUNRES)))
names(TGD_37_41) = c("CODMUNRES","TGD_37_41")
base = merge(base, TGD_37_41, by="CODMUNRES", all.x=TRUE)

# TGD_42 - total de gestações com duração >=42
TGD_42 = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$SEMAGESTAC >= 42],
         levels = base$CODMUNRES)))
names(TGD_42) = c("CODMUNRES","TGD_42")
base = merge(base, TGD_42, by="CODMUNRES", all.x=TRUE)

# TGD_PRT - total de gestações pré-termo, duração < 37 semanas
TGD_PRT = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$SEMAGESTAC < 37],
         levels = base$CODMUNRES)))
names(TGD_PRT) = c("CODMUNRES","TGD_PRT")
base = merge(base, TGD_PRT, by="CODMUNRES", all.x=TRUE)

# TGD_AT - total de gestações a termo, duração >=37 e <=41
TGD_AT = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[
    dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41],
    levels = base$CODMUNRES)))
names(TGD_AT) = c("CODMUNRES","TGD_AT")
base = merge(base, TGD_AT, by="CODMUNRES", all.x=TRUE)

# TGD_PST - total de gestações pós termo, duração >=42
TGD_PST = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$SEMAGESTAC >= 42],
         levels = base$CODMUNRES)))
names(TGD_PST) = c("CODMUNRES","TGD_PST")
base = merge(base, TGD_PST, by="CODMUNRES", all.x=TRUE)

# DG_P25 - percentil 25 da duração da gestação
DG_P25 = aggregate(SEMAGESTAC ~ CODMUNRES,
                   data = dados_sinasc_2,
                   function(x) quantile(x, 0.25, na.rm = TRUE))
names(DG_P25) = c("CODMUNRES","DG_P25")
base = merge(base, DG_P25, by = "CODMUNRES", all.x = TRUE)

# DG_P50 - percentil 50 da duração da gestação
DG_P50 = aggregate(SEMAGESTAC ~ CODMUNRES,
                   data = dados_sinasc_2,
                   function(x) quantile(x, 0.5, na.rm = TRUE))
names(DG_P50) = c("CODMUNRES","DG_P50")
base = merge(base, DG_P50, by = "CODMUNRES", all.x = TRUE)

# DG_P75 - percentil 75 da duração da gestação
DG_P75 = aggregate(SEMAGESTAC ~ CODMUNRES,
                   data = dados_sinasc_2,
                   function(x) quantile(x, 0.75, na.rm = TRUE))
names(DG_P75) = c("CODMUNRES","DG_P75")
base = merge(base, DG_P75, by = "CODMUNRES", all.x = TRUE)

# DG_MD - duração média da gestação
DG_MD = aggregate(SEMAGESTAC ~ CODMUNRES, data = dados_sinasc_2, mean, na.rm = TRUE)
names(DG_MD) = c("CODMUNRES","DG_MD")
base = merge(base, DG_MD, by = "CODMUNRES", all.x = TRUE)
base$DG_MD = round(base$DG_MD, 2)

# DG_DP - desvio-padrão da duração da gestação
DG_DP = aggregate(SEMAGESTAC ~ CODMUNRES, data = dados_sinasc_2, mean, na.rm = TRUE)
names(DG_DP) = c("CODMUNRES","DG_DP")
base = merge(base, DG_DP, by = "CODMUNRES", all.x = TRUE)
base$DG_DP = round(base$DG_DP, 2)

# TKC_NR - total de consultas de pre-natal não realizado
TKC_NR = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$KOTELCHUCK == "Não realizou pré-natal"],
                                    levels = base$CODMUNRES)))
names(TKC_NR) = c("CODMUNRES","TKC_NR")
base = merge(base, TKC_NR, by = "CODMUNRES", all.x = TRUE)

# TKC_ID - total de consultas de pre-natal inadequado
TKC_ID = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$KOTELCHUCK == "Inadequado"],
                                    levels = base$CODMUNRES)))
names(TKC_ID) = c("CODMUNRES","TKC_ID")
base = merge(base, TKC_ID, by = "CODMUNRES", all.x = TRUE)

# TKC_IT - total de consultas de pre-natal intermediário
TKC_IT = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$KOTELCHUCK == "Intermediário"],
                                    levels = base$CODMUNRES)))
names(TKC_IT) = c("CODMUNRES","TKC_IT")
base = merge(base, TKC_IT, by = "CODMUNRES", all.x = TRUE)

# TKC_AD- total de consultas de pre-natal adequado
TKC_AD = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$KOTELCHUCK == "Adequado"],
                                    levels = base$CODMUNRES)))
names(TKC_AD) = c("CODMUNRES","TKC_AD")
base = merge(base, TKC_AD, by = "CODMUNRES", all.x = TRUE)

# TKC_MAD - total de consultas de pre-natal mais que adequado
TKC_MAD = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$KOTELCHUCK == "Mais que adequado"],
                                    levels = base$CODMUNRES)))
names(TKC_MAD) = c("CODMUNRES","TKC_MAD")
base = merge(base, TKC_MAD, by = "CODMUNRES", all.x = TRUE)

# TGPRG_S - total de gestantes que peregrinaram
TGPRG_S = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PERIG == "Sim"],
                                     levels = base$CODMUNRES)))
names(TGPRG_S) = c("CODMUNRES","TGPRG_S")
base = merge(base, TGPRG_S, by = "CODMUNRES", all.x = TRUE)

# TGPRG_N - total de gestantes que não peregrinaram
TGPRG_N = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PERIG == "Não"],
                                     levels = base$CODMUNRES)))
names(TGPRG_N) = c("CODMUNRES","TGPRG_N")
base = merge(base, TGPRG_N, by = "CODMUNRES", all.x = TRUE)

# TPV - total de partos vaginais
TPV = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PARTO == "Vaginal"],
                                     levels = base$CODMUNRES)))
names(TPV) = c("CODMUNRES","TPV")
base = merge(base, TPV, by = "CODMUNRES", all.x = TRUE)

# TPC - total de partos cesáreos
TPC = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PARTO == "Cesário"],
                                 levels = base$CODMUNRES)))
names(TPC) = c("CODMUNRES","TPC")
base = merge(base, TPC, by = "CODMUNRES", all.x = TRUE)

# TRAP_C - total de recém-nascidos na posição cefálica
TRAP_C = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$TPAPRESENT == "Cefálico"],
                                 levels = base$CODMUNRES)))
names(TRAP_C) = c("CODMUNRES","TRAP_C")
base = merge(base, TRAP_C, by = "CODMUNRES", all.x = TRUE)

# TRAP_P - total de recém-nascidos na posição pélvica ou podálica
TRAP_P = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$TPAPRESENT == "Pélvica ou podálica"],
                                    levels = base$CODMUNRES)))
names(TRAP_P) = c("CODMUNRES","TRAP_P")
base = merge(base, TRAP_P, by = "CODMUNRES", all.x = TRUE)

# TRAP_T - total de recém-nascidos na posição transversa
TRAP_T = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$TPAPRESENT == "Transversa"],
                                    levels = base$CODMUNRES)))
names(TRAP_T) = c("CODMUNRES","TRAP_T")
base = merge(base, TRAP_T, by = "CODMUNRES", all.x = TRUE)

# TGROB_1 - total de gestantes do grupo de Robson 1
# TGROB_2 - total de gestantes do grupo de Robson 2
# TGROB_3 - total de gestantes do grupo de Robson 3
# TGROB_4 - total de gestantes do grupo de Robson 4
# TGROB_5 - total de gestantes do grupo de Robson 5
# TGROB_6 - total de gestantes do grupo de Robson 6
# TGROB_7 - total de gestantes do grupo de Robson 7
# TGROB_8 - total de gestantes do grupo de Robson 8
# TGROB_9 - total de gestantes do grupo de Robson 9
# TGROB_10 - total de gestantes do grupo de Robson 10
tab_robson = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$TPROBSON)
tab_robson = as.data.frame.matrix(tab_robson)
colnames(tab_robson) = paste0("TGROB_", 1:10)
tab_robson$CODMUNRES = rownames(tab_robson)
base = merge(base, tab_robson, by = "CODMUNRES", all.x = TRUE)

# TNLOC_H - total de nascimentos em hospital
# TNLOC_ES - total de nascimentos em outros estabelecimentos de saúde
# TNLOC_D - total de nascimentos em domicílio
# TNLOC_O - total de nascimentos em outros locais
tab_loc = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$LOCNASC)
tab_loc = as.data.frame.matrix(tab_loc)
colnames(tab_loc) = c("TNLOC_H","TNLOC_ES","TNLOC_D","TNLOC_O")
tab_loc$CODMUNRES = rownames(tab_loc)
base = merge(base, tab_loc, by = "CODMUNRES", all.x = TRUE)

# TNLOC_AI - total de nascimentos em aldeias indígenas
TNLOC_AI = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$LOCNASC == "Aldeia indígena"],
                                    levels = base$CODMUNRES)))
names(TNLOC_AI) = c("CODMUNRES","TNLOC_AI")
base = merge(base, TNLOC_AI, by = "CODMUNRES", all.x = TRUE)

# TRS_M - total de recém-nascidos do sexo masculino
# TRS_F - total de recém-nascidos do sexo feminino
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c("Masculino","Feminino"))
tab_sexo = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$SEXO)
tab_sexo = as.data.frame.matrix(tab_sexo)
colnames(tab_sexo) = c("TRS_M","TRS_F")
tab_sexo$CODMUNRES = rownames(tab_sexo)
base = merge(base, tab_sexo, by = "CODMUNRES", all.x = TRUE)

# TRRC_B - total de recém-nascidos da raça/cor branca
# TRRC_PT - total de recém-nascidos da raça/cor preta
# TRRC_A - total de recém-nascidos da raça/cor amarela
# TRRC_PD - total de recém-nascidos da raça/cor parda
# TRRC_I - total de recém-nascidos da raça/cor indígena
dados_sinasc_2$RACACOR <- factor(dados_sinasc_2$RACACOR,
                                 levels = c("Branca","Preta","Amarela","Parda","Indígena"))
tab_raca_rn = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$RACACOR)
tab_raca_rn = as.data.frame.matrix(tab_raca_rn)
colnames(tab_raca_rn) = c("TRRC_B","TRRC_PT","TRRC_A","TRRC_PD","TRRC_I")
tab_raca_rn$CODMUNRES = rownames(tab_raca_rn)
base = merge(base, tab_raca_rn, by = "CODMUNRES", all.x = TRUE)

# TRP_BP - total de recém-nascidos com baixo peso, peso < 2500
# TRP_N - total de recém-nascidos com peso normal, peso >= 2500 e < 4000
# TRP_M - total de recém-nascidos com macrossomia, peso >= 4000
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia"))
tab_peso = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$F_PESO)
tab_peso = as.data.frame.matrix(tab_peso)
colnames(tab_peso) = c("TRP_BP","TRP_N","TRP_M")
tab_peso$CODMUNRES = rownames(tab_peso)
base = merge(base, tab_peso, by = "CODMUNRES", all.x = TRUE)

# PESO_P25 - percentil 25 do peso dos recém-nascidos
PESO_P25 = aggregate(PESO ~ CODMUNRES,
                     data = dados_sinasc_2,
                     function(x) quantile(x, 0.25, na.rm = TRUE))
names(PESO_P25) = c("CODMUNRES","PESO_P25")
base = merge(base, PESO_P25, by = "CODMUNRES", all.x = TRUE)

# PESO_P50 - percentil 50 do peso dos recém-nascidos
PESO_P50 = aggregate(PESO ~ CODMUNRES,
                     data = dados_sinasc_2,
                     function(x) quantile(x, 0.5, na.rm = TRUE))
names(PESO_P50) = c("CODMUNRES","PESO_P50")
base = merge(base, PESO_P50, by = "CODMUNRES", all.x = TRUE)

# PESO_P75 - percentil 75 do peso dos recém-nascidos
PESO_P75 = aggregate(PESO ~ CODMUNRES,
                     data = dados_sinasc_2,
                     function(x) quantile(x, 0.75, na.rm = TRUE))
names(PESO_P75) = c("CODMUNRES","PESO_P75")
base = merge(base, PESO_P75, by = "CODMUNRES", all.x = TRUE)

# PESO_MD - peso médio dos recém-nascidos
PESO_MD = aggregate(PESO ~ CODMUNRES, data = dados_sinasc_2, mean, na.rm = TRUE)
names(PESO_MD) = c("CODMUNRES","PESO_MD")
base = merge(base, PESO_MD, by = "CODMUNRES", all.x = TRUE)
base$PESO_MD = round(base$PESO_MD, 2)

# PESO_DP - desvio-padrão dos pesos dos recém-nascidos
PESO_DP = aggregate(PESO ~ CODMUNRES, data = dados_sinasc_2, sd, na.rm = TRUE)
names(PESO_DP) = c("CODMUNRES","PESO_DP")
base = merge(base, PESO_DP, by = "CODMUNRES", all.x = TRUE)
base$PESO_DP = round(base$PESO_DP, 2)

# TRPIG_P - total de recém-nascidos de GESTAÇÕES ÚNICAS com PIG
# TRPIG_A - total de recém-nascidos de GESTAÇÕES ÚNICAS com AIG
# TRPIG_G - total de recém-nascidos de GESTAÇÕES ÚNICAS com GIG
base_unica = dados_sinasc_2[dados_sinasc_2$GRAVIDEZ == "Única", ]
base_unica$F_PIG = factor(base_unica$F_PIG, levels = c("PIG","AIG","GIG"))
tab_pig = table(base_unica$CODMUNRES, base_unica$F_PIG)
tab_pig = as.data.frame.matrix(tab_pig)
colnames(tab_pig) = c("TRPIG_P","TRPIG_A","TRPIG_G")
tab_pig$CODMUNRES = rownames(tab_pig)
base = merge(base, tab_pig, by = "CODMUNRES", all.x = TRUE)

# TRAPG5_B - total de recém-nascidos com Apgar5 baixo, < 7
TRAPG5_B = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$APGAR5 < 7],
         levels = base$CODMUNRES)))
names(TRAPG5_B) = c("CODMUNRES","TRAPG5_B")
base = merge(base, TRAPG5_B, by="CODMUNRES", all.x=TRUE)

# TRAPG5_N - total de recém-nascidos com Apgar5 normal, >= 7
TRAPG5_N = as.data.frame(table(
  factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$APGAR5 >= 7],
         levels = base$CODMUNRES)))
names(TRAPG5_N) = c("CODMUNRES","TRAPG5_N")
base = merge(base, TRAPG5_N, by="CODMUNRES", all.x=TRUE)

# APG5_MD - Apgar5 médio dos recém-nascidos
APG5_MD = aggregate(APGAR5 ~ CODMUNRES, data = dados_sinasc_2, mean, na.rm = TRUE)
names(APG5_MD) = c("CODMUNRES","APG5_MD")
base = merge(base, APG5_MD, by = "CODMUNRES", all.x = TRUE)
base$APG5_MD = round(base$APG5_MD, 2)

# APG5_DP - desvio-padrão dos Apgar5 dos recém-nascidos
APG5_DP = aggregate(APGAR5 ~ CODMUNRES, data = dados_sinasc_2, sd, na.rm = TRUE)
names(APG5_DP) = c("CODMUNRES","APG5_DP")
base = merge(base, APG5_DP, by = "CODMUNRES", all.x = TRUE)
base$APG5_DP = round(base$APG5_DP, 2)

# TRAC - total de recém-nascidos com anomalia congênita
# TRSAC - total de recém-nascidos sem anomalia congênita
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, labels = c("Sim","Não"))
tab_anom = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$IDANOMAL)
tab_anom = as.data.frame.matrix(tab_anom)
colnames(tab_anom) = c("TRAC","TRSAC")
tab_anom$CODMUNRES = rownames(tab_anom)
base = merge(base, tab_anom, by="CODMUNRES", all.x=TRUE)

base = cbind(ANO = 2015, NIVEL = "MUNICIPIO", base)

# Selecionar variáveis numéricas (tudo menos identificação)
vars = names(base)[!(names(base) %in% c("ANO","NIVEL","CODMUNRES"))]

# Somar variáveis
UF = colSums(base[, vars], na.rm = TRUE)
UF = data.frame(t(UF))

# Corrigir medidas
UF$IM_P25 = quantile(dados_sinasc_2$IDADEMAE, 0.25, na.rm = TRUE)
UF$IM_P50 = quantile(dados_sinasc_2$IDADEMAE, 0.50, na.rm = TRUE)
UF$IM_P75 = quantile(dados_sinasc_2$IDADEMAE, 0.75, na.rm = TRUE)
UF$IM_MD = round(mean(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
UF$IM_DP = round(sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
UF$DG_P25 = quantile(dados_sinasc_2$SEMAGESTAC, 0.25, na.rm = TRUE)
UF$DG_P50 = quantile(dados_sinasc_2$SEMAGESTAC, 0.5, na.rm = TRUE)
UF$DG_P75 = quantile(dados_sinasc_2$SEMAGESTAC, 0.75, na.rm = TRUE)
UF$DG_MD = round(mean(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE), 2)
UF$DG_DP = round(sd(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE), 2)
UF$PESO_P25 = quantile(dados_sinasc_2$PESO, 0.25, na.rm = TRUE)
UF$PESO_P50 = quantile(dados_sinasc_2$PESO, 0.5, na.rm = TRUE)
UF$PESO_P75 = quantile(dados_sinasc_2$PESO, 0.75, na.rm = TRUE)
UF$PESO_MD = round(mean(dados_sinasc_2$PESO, na.rm = TRUE), 2)
UF$PESO_DP = round(sd(dados_sinasc_2$PESO, na.rm = TRUE), 2)
UF$APG5_MD = round(mean(dados_sinasc_2$APGAR5, na.rm = TRUE), 2)
UF$APG5_DP = round(sd(dados_sinasc_2$APGAR5, na.rm = TRUE), 2)

# Adicionar colunas fixas
UF$ANO = 2015
UF$NIVEL = "UF"
UF$CODMUNRES = 27   # AL

# Organizar colunas igual à base
UF = UF[, names(base)]

# Colocar UF como primeira linha
SINASC_AL = rbind(UF, base)

# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv
write.csv(SINASC_AL, "SINASC_AL.csv", row.names = FALSE)

# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"

##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

