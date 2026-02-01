if(!require(readr)) install.packages("readr")

library(readr)

dados <- read_csv2("C:/R_Vigilancia/e_sus_notifica.csv")

if(!require(foreign)) install.packages("foreign")

library(foreign)

dados_sinan <- read.dbf("C:/R_Vigilancia/NINDINET.dbf", as.is = TRUE)

head(dados_sinan)

if(!require(readxl)) install.packages("readxl")

library(readxl)

dados_sivep <- read_excel("C:/R_Vigilancia/sivep_gripe.xlsx",
                          sheet = "SIVEPGRIPE",
                          skip = 0)

head(dados_sivep)

sapply(X = dados_sivep, FUN = "typeof")

dados_sivep$CS_CLASSI_FIN_N <- factor(
  x = dados_sivep$CLASSI_FIN,
  levels = c("1", "2", "3", "4", "5"),
  labels = c(
    "SRAG por influenza",
    "SRAG por outro vírus respiratório",
    "SRAG por outro agente etiológico",
    "SRAG não especificado",
    "SRAG por COVID-19"
  )
)

dados_sivep$CS_ESCOL_N <- factor(
  x = dados_sivep$CS_ESCOL_N,
  levels = c("0", "1", "2", "3", "4", "5", "9"),
  labels = c(
    "Sem escolaridade",
    "Fundamental 1º ciclo",
    "Fundamental 2º ciclo",
    "Médio",
    "Superior",
    "Não se aplica",
    "Ignorado"
  ),
  ordered = TRUE
)

if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(janitor)) install.packages("janitor");library(janitor)
if(!require(skimr)) install.packages("skimr");library(skimr)
if(!require(stringr)) install.packages("stringr");library(stringr)
if(!require(stringi)) install.packages("stringi");library(stringi)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)

if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)

library(tidyverse)

base <- read.dbf('C:/R_Vigilancia/NINDINET.dbf', as.is = TRUE)

glimpse(base)

base_menor <- select(base, DT_NOTIFIC, DT_NASC, CS_SEXO, CS_RACA, ID_MN_RESI, 
                     ID_AGRAVO)

head(base_menor)

# Utilizando a função `head()` aninhada com a função `select()`
head(select(base, DT_NOTIFIC, DT_NASC, CS_SEXO, CS_RACA, ID_MN_RESI, ID_AGRAVO))

# Criando o objeto {`base_menor`} a partir do objeto {`base`} e
# inserindo o pipe `|>` para encadear a seleção de variáveis
base_menor <- base |>
  select(DT_NOTIFIC, DT_NASC, CS_SEXO, CS_RACA, ID_MN_RESI, ID_AGRAVO) |>
  head()
base_menor

base_menor |>
  
  # Excluindo a variável "DT_NASC" da tabela {`base_menor`}
  select(-DT_NASC) |>
  
  # Utilizando a função `head()` para visualizar as primeiras linhas da tabela 
  # após a exclusão
  head()

# Criando a {`base_menor_2`}
base_menor_2 <- base |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(NU_NOTIFIC, ID_AGRAVO, DT_NOTIFIC, DT_DIGITA) |>
  
  # Utilizando a função `mutate()` para criar a nova coluna
  mutate(TEMPO_DIGITA = DT_DIGITA - DT_NOTIFIC)

base_menor_2 |>
  
  # Transformando a coluna "TEMPO_DIGITA" da tabela {`base_menor_2`} 
  # no tipo numérico com a função `as.numeric()` dentro da função `mutate()`
  mutate(TEMPO_DIGITA = as.numeric(TEMPO_DIGITA)) |>
  
  # Utilizando a função `head()` para visualizar as primeiras linhas
  head()

cid10_categorias <- read_csv2('C:/R_Vigilancia/CID-10-CATEGORIAS.CSV')

colnames(cid10_categorias)

cid10_categorias_nova <- clean_names(cid10_categorias)

colnames(cid10_categorias_nova)

base_menor_2 |>
  
  # Filtrando os CIDs igual a B19, da coluna "ID_AGRAVO"
  # com tempo de digitação maior que sete dias
  filter(ID_AGRAVO == "B19", TEMPO_DIGITA > 7) |>
  
  # Utilizando a função `head()` para visualizar as primeiras linhas da tabela
  head()

base_menor_2 |>
  
  # Filtrando os registros iguais a B19 OU A279 OU B54 na coluna "ID_AGRAVO"
  filter(ID_AGRAVO == "B19" | ID_AGRAVO == "A279" | ID_AGRAVO == "B54") |>
  
  # Utilizando a função `head()` para visualizar as primeiras 20 linhas da tabela
  head(20)

base_menor_2 |>
  
  # Filtrando os agravos utilizando o operador `%in%`
  filter(ID_AGRAVO %in% c("B19", "A279", "B54")) |>
  
  # Utilizando a função `head()` para visualizar as primeiras 20 linhas da tabela
  head(20)

base_menor_2 |>
  
  # Agrupando as notificações pelos agravos
  group_by(ID_AGRAVO) |>
  
  # Contando a frequência de notificações por agravos
  count(ID_AGRAVO)

base_menor_2 |>
  
  # Agrupando as notificações pelos agravos
  group_by(ID_AGRAVO) |>
  
  # Contando a frequência de notificações por agravos
  count(ID_AGRAVO) |>
  
  # Visualizando as 20 primeiras linhas da tabela 
  # {`base_menor_2`} 
  # usando a função `print()`
  print (n = 20)

base_menor_2 |>
  
  # Agrupando as notificações pelos agravos
  group_by(ID_AGRAVO) |>
  
  # Utilizando a função `summarise()` para criar novas colunas de síntese
  summarise(
    
    # Criando uma coluna de total, utilizando a função `n()`
    total_agravos =  n(),
    
    # Criando uma coluna de média, utilizando a função `mean()`
    media_digita = mean(TEMPO_DIGITA)
  )

base_menor_2 |>
  
  # Agrupando as notificações pelos agravos
  group_by(ID_AGRAVO) |>
  
  # Utilizando a função `summarise()` para criar novas colunas de síntese
  summarise(total_agravos =  n(),
            media_digita = mean(TEMPO_DIGITA)) |>
  
  # Ordenando a tabela pela ordem decrescente da média de tempo de digitação
  arrange(desc(media_digita))

base |>
  summarise(
    total_completo = sum(!is.na(CS_RACA)),
    total_registros = n(),
    total_missing_raca = sum(is.na(CS_RACA))
  ) |>
  mutate(
    taxa_completude = (total_completo / total_registros) * 100
  )

base |>
  
  # Utilizando a função `summarise()` para criar novas 
  # colunas de síntese
  summarise(
    
    # Criando uma coluna de total de registros com a 
    # variável raça/cor devidamente preenchida
    total_completo = n_complete(CS_RACA),
    
    # Criando uma coluna de total de registros da 
    # variável raça/cor em branco
    total_na = n_missing(CS_RACA),
    
    # Criando uma coluna de taxa de completude
    taxa_completude = complete_rate(CS_RACA) * 100
  )    

base |>
  
  # Contabilizando o número de registros conforme as categorias de preenchimento
  # da variável "CS_RACA"
  count(CS_RACA)

base |>
  
  # Transformando os valores em branco em 9 na coluna "CS_RACA"
  mutate(CS_RACA = replace_na(CS_RACA, replace = 9)) |>
  
  # Contabilizando o número de registros conforme as categorias de preenchimento
  # da variável "CS_RACA"
  count(CS_RACA)

base |>
  mutate(CS_RACA = na_if(CS_RACA, 9)) |>
  count(CS_RACA)

dados <- read_csv("C:/R_Vigilancia/cobertura_hepatiteb_rosas_2016_2020_A.csv")

glimpse(dados)

# realizando a transposição dos dados do formato LARGO (*wide*) 
# para uma tabela em formato LONGO (*long*) 
dados |>
  
  # Utilizando a função `pivot_longer()` para transformação de colunas
  pivot_longer(
    
    # Definindo as colunas que serão transformadas
    cols = c("Ano 2016","Ano 2017","Ano 2018","Ano 2019","Ano 2020"),
    
    # Definindo o nome da variável nova que receberá os nomes acima
    names_to = "Ano",
    
    # Definindo o nome da variável nova que receberá os valores da tabela
    values_to = "Cobertura Vacinal contra Hepatite B",
  )

library(tidyr)
library(dplyr)

# Criando o objeto {`dados_longos`}
dados_longos <- dados |>
  
  # Utilizando a função `pivot_longer()` para transformação de colunas
  pivot_longer(
    
    # Definindo as colunas que serão transformadas
    cols = c("Ano 2016", "Ano 2017", "Ano 2018", "Ano 2019", "Ano 2020"),
    
    # Definindo o nome da variável nova que receberá os nomes acima
    names_to = "Ano",
    
    # Definindo o nome da variável nova que receberá os valores da tabela
    values_to = "Cobertura Vacinal contra Hepatite B",
    
    # Retirando a palavra "Ano " antes de cada valor da variável Ano
    # Também estamos retirando o espaço depois da palavra "Ano"
    names_prefix = "Ano "
  )

# Visualizando a tabela {`dados_longos`} no formato longo
dados_longos

dados <- read_csv("C:/R_Vigilancia/cobertura_hepatiteb_rosas_2016_2020_A.csv")

# realizando a transposição dos dados do formato LARGO (*wide*) 
# para uma tabela em formato LONGO (*long*) 
dados |>
  
  # Utilizando a função `pivot_longer()` para transformação de colunas
  pivot_longer(
    
    # Definindo as colunas que serão transformadas
    cols = c("Ano 2016","Ano 2017","Ano 2018","Ano 2019","Ano 2020"),
    
    # Definindo o nome da variável nova que receberá os nomes acima
    names_to = "Ano",
    
    # Definindo o nome da variável nova que receberá os valores da tabela
    values_to = "Cobertura Vacinal contra Hepatite B",
  )

# Criando o objeto {`dados_longos`}
dados_longos <- dados |>
  
  # Utilizando a função `pivot_longer()` para transformação de colunas
  pivot_longer(
    
    # Definindo as colunas que serão transformadas
    cols = c("Ano 2016", "Ano 2017", "Ano 2018", "Ano 2019", "Ano 2020"),
    
    # Definindo o nome da variável nova que receberá os nomes acima
    names_to = "Ano",
    
    # Definindo o nome da variável nova que receberá os valores da tabela
    values_to = "Cobertura Vacinal contra Hepatite B",
    
    # Retirando a palavra "Ano " antes de cada valor da variável Ano
    # Também estamos retirando o espaço depois da palavra "Ano"
    names_prefix = "Ano "
  )

# Visualizando a tabela {`dados_longos`} no formato longo
dados_longos

# Transndo os dados no formato longo em formato largo
# Criando o objeto {`dados_largos`}
dados_largos <- dados_longos |>
  
  # Utilizando a função `pivot_wider()` para transformação de colunas
  pivot_wider(
    
    # Definindo de qual variável estamos resgatando os nomes das colunas
    names_from = "Ano",
    
    # Definindo de qual variável estamos resgatando os valores das colunas
    values_from = "Cobertura Vacinal contra Hepatite B")

# visualizando a tabela
dados_largos

eapv_2021m <- read_xlsx('C:/R_Vigilancia/notificacao_eapv_2021m.xlsx')

View(eapv_2021m)

eapv_2021m |>
  
  # Selecionando três colunas do data.frame {`eapv_2021m`}
  select(imunobiologico_vacina, dose, data_da_aplicacao) |>
  
  # Utilizando a função `head()` para visualizar as primeiras linhas da tabela
  head()

eapv_2021m |>
  
  # Dividindo a coluna `imunobiologico_vacina` em três novas colunas
  separate(
    
    # Definindo a coluna que será separada
    col = imunobiologico_vacina,
    
    # Definindo os nomes das novas colunas
    into = c("vac_event_1", "vac_event_2", "vac_event_3"),
    
    # Definindo qual o caractere que está sendo utilizado dentro das colunas
    sep = "\\|"
  ) |>
  
  # Selecionando as novas colunas para visualização
  select("vac_event_1", "vac_event_2", "vac_event_3")

base_menor |>
  
  # Renomeando os valores da variável CS_SEXO usando a função `mutate()` e 
  # `case_when()`
  mutate(
    sexo_cat = case_when(
      CS_SEXO == "M" ~ "Masculino",
      CS_SEXO == "F" ~ "Feminino",
      CS_SEXO == "I" | is.na(CS_SEXO) ~ "Ignorado",
      TRUE ~ NA_character_
    )
  ) |>
  
  # Visualizando a tabela {`base_menor`} recodificada
  head()

base_menor |>
  
  # Renomeando os valores da variável CS_RACA usando a função `mutate()` e 
  # `case_when()`
  mutate(
    raca_cor_cat = case_when(
      
      # Se o valor da coluna for igual a "1" transforme para "Branca"
      CS_RACA == "1" ~ "Branca",
      
      # Se o valor da coluna for igual a "2" transforme para "Preta"
      CS_RACA == "2" ~ "Preta",
      
      # Se o valor da coluna for igual a "3" transforme para "Amarela"
      CS_RACA == "3" ~ "Amarela",
      
      # Se o valor da coluna for igual a "4" transforme para "Parda"
      CS_RACA == "4" ~ "Parda",
      
      # Se o valor da coluna for igual a "5" transforme para "Indígena"
      CS_RACA == "5" ~ "Indígena",
      
      # Se o valor da coluna for igual a "9" ou nulo transforme para "Ignorado"
      CS_RACA == "9" | is.na(CS_RACA) ~ "Ignorado",
      
      # Caso acontecer um valor diferente dos citados acima, transforme para "NA"
      TRUE ~ NA_character_
    )
  ) |>
  
  # Utilizando a função `head()` para visualizar as primeiras linhas
  head()

base |>
  
  # Utilizando a função `mutate()` para criar colunas
  mutate(
    
    # Criando uma coluna de idade conforme a codificação da variável NU_IDADE_N
    idade_anos = if_else(str_sub(NU_IDADE_N, 1, 1) == "4", 
                         as.numeric(str_sub(NU_IDADE_N, 2, 4)), 0),
    
    # Criando uma coluna de faixa etária a partir da variável idade dos casos 
    # notificados
    # utilizando a função `cut()`
    fx_etaria = cut(
      # Definindo qual variável será classificada em faixas
      x = idade_anos,
      
      # Definindo os pontos de corte das classes
      breaks = c(0, 10, 20, 60, Inf),
      
      # Definindo os rótulos das classes
      labels = c("0-9 anos", "10-19 anos", "20-59 anos", "60 anos e+"),
      
      # Definindo o tipo do ponto de corte
      right = FALSE
    )
  ) |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(NU_NOTIFIC, ID_AGRAVO, NU_IDADE_N, idade_anos, fx_etaria) |>
  
  # Visualizando a tabela {`base`} modificada
  head()

# Importando o banco de dados {`tabela_municipios.xlsx`} para o `R`
tabela_municipios <- read_excel("C:/R_Vigilancia/tabela_municipios.xlsx",
                                sheet = 1,
                                skip = 0)

# Verificando o tipo de variável na coluna `ID_MN_RESI` do NINDINET
class(base_menor$ID_MN_RESI)

# Verificando o tipo de variável na coluna `ID_MUNICIPIO` da tabela de municípios 
# do IBGE
class(tabela_municipios$ID_MUNICIPIO)

# Transformando apenas a variável `ID_MUNICIPIO` do data.frame {`tabela_municipios`}
# e utilizando a função `as.integer()` para torná-la do tipo numérica 
tabela_municipios$ID_MUNICIPIO <- as.integer(tabela_municipios$ID_MUNICIPIO)

class(tabela_municipios$ID_MUNICIPIO)

left_join(
  
  # Unindo a tabela {`base_menor`}
  x = base_menor,
  
  # com a tabela {`tabela_municipios`}
  y = tabela_municipios,
  
  # Selecionando as colunas `ID_MN_RESI` e `ID_MUNICIPIO` para unir os bancos de 
  # dados
  by = c("ID_MN_RESI" = "ID_MUNICIPIO")) |>
  
  # Visualizando a união realizada
  head()

# Importando o banco de dados { `CID-10-CATEGORIAS.CSV` } para o `R`
CID10 <- read_csv2("C:/R_Vigilancia/CID-10-CATEGORIAS.CSV",
                   locale = locale(encoding = "latin1"))

# Criando um novo objeto com as seleções feitas
nomes_cid <- CID10 |>
  
  # Selecionar apenas as linhas de interesse (linhas 1 a 15)
  slice(1:15) |> 
  
  # Selecionando apenas a variável "DESCRICAO" e transformando em `character` 
  # simples
  pull(DESCRICAO)

# Contando quantos caracteres existem em cada linha
str_length(nomes_cid)

# Extraindo caracteres entre a 1ª e a 11ª posição da linha
str_sub(nomes_cid, start = 1, end = 11)

# Indicando a posição final da primeira palavra de cada texto
str_sub(nomes_cid, 
        start = 1, 
        end = c(6,6,6,10,6,6,8,6,9,8,11,11,11,11,11))

# Ordenando o objeto {`nomes_cid`} para o formato ascendente (A-Z)
str_sort(nomes_cid)

# Ordenando o objeto {`nomes_cid`} para o formato decrescente (Z-A)
str_sort(nomes_cid, decreasing = TRUE)

# Unindo *strings* para a formação da palavra "Tuberculose"
str_c("Tuber","culo","se")

# Unindo *strings* incluindo espaço entre elas
# adicionaremos o argumento `sep = " "`
str_c("Tuberculose","respiratória,","com","confirmação",
      "bacteriológica","e histológica", sep = " ")

# Salvando as modificações no objeto {`agravo`}
agravo <- c("Tuberculose","respiratória,","com","confirmação",
            "bacteriológica","e histológica")

# Unindo *strings* armazenadas no objeto {`agravo`}
# em um único *string* de texto
str_c(agravo, collapse = " ")

# Transformando todos as letras em maiúsculas
str_to_upper(nomes_cid)

# Transformando todos as letras em minúsculas
str_to_lower(nomes_cid)

# Transformando apenas a primeira maiúscula e restante das palavras em minúsculas
str_to_sentence(nomes_cid)

# Vetor com diferentes grafias
nomes <- c("infecções", "infeccões", "infecçoes", "infeccoes")

# Transformação das letras especiais ou com acentuação
stri_trans_general(nomes, id = "Latin-ASCII")

# Extraindo linhas onde a palavra "Tuberculose" aparece
str_subset(nomes_cid, pattern = "Tuberculose")

# Extraindo linhas onde a palavra "tuberculose" aparece, independente se
# maiúscula ou minúscula
str_subset(nomes_cid, pattern = fixed("tuberculose", ignore_case = TRUE))

CID10 |> 
  
  # Utilizando a função `filter()` com a função 
  # `str_stars()`
  filter(
    str_starts(
      
      # Selecionando a variável usada para encontrar 
      # palavras no início da frase
      string = DESCRICAO,
      
      # Definindo a palavra a ser pesquisada e ignorando 
      # se maiúscula ou minúscula
      pattern = fixed("outras", ignore_case = TRUE)
    )
  ) |> 
  
  # Selecionando as variáveis que queremos utilizar 
  # com a função `select()`
  select(CAT, DESCRICAO)

CID10 |> 
  
  # Utilizando a função `filter()` com a função 
  # `str_ends()`
  filter(
    str_ends(
      
      # Definindo a variável usada para encontrar 
      # palavras no final da frase
      string = DESCRICAO,
      
      # Definindo a palavra a ser pesquisada e ignorando 
      # se maiúscula ou minúscula
      pattern = fixed("bacterianas", ignore_case = TRUE)
    )
  ) |> 
  
  # Selecionando as variáveis que queremos utilizar 
  # com a função `select()`
  select(CAT, DESCRICAO)

# Criando um vetor de texto
agravo <- "Tuberculose respiratória, com confirmação bacteriológica e histológica"

# Substituindo palavras
str_replace(agravo, pattern = "com", replacement = "sem")

# Criando um vetor de texto
agravo <- "Tuberculose respiratória, com confirmação bacteriológica e histológica"

# Substituindo palavras
str_replace_all(agravo, pattern = "ó", replacement = "o")

# Transformando uma string em data
data_1 <- as_date("2022-05-19")

# Visualizando a string transformada em data
data_1

# Verificando a classe do objeto `data_1`
class(data_1)

# Transformando uma string em data definindo o formato
as_date("19/05/2022", format = "%d/%m/%Y")

# Transformando uma string do tipo das datas do SIM definindo o formato
as_date("01031998", format = "%d%m%Y")

# Importando o banco de dados { `NINDINET.dbf` } para o `R`
dt_notific <- read.dbf('C:/R_Vigilancia/NINDINET.dbf') |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NOTIFIC, DT_SIN_PRI, DT_NASC) |>
  
  # Selecionar apenas as linhas de interesse (linhas 1 a 10)
  slice(1:10)

# Somando 60 dias às datas de notificação de casos da tabela {`dt_notific`}
dt_notific$DT_NOTIFIC + 60

# Subtraindo 60 dias as datas de notificação 
#de casos à tabela {`dt_notific`}
dt_notific$DT_NOTIFIC - 60

# Criando a tabela {`dt_notific_2`}
dt_notific_2 <- dt_notific |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NOTIFIC) |>
  
  # Utilizando a função `mutate()` para criar a nova coluna "prazo_encerramento"
  mutate(prazo_encerramento = DT_NOTIFIC + 60)

# Visualizando a tabela {`dt_notific_2`} criada
dt_notific_2

# Criando a tabela {`dif_tempo`}
dif_tempo <- dt_notific |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NOTIFIC, DT_SIN_PRI) |>
  
  # Utilizando a função `mutate()` para criar a nova coluna "DIFERENCA"
  mutate(DIFERENCA = DT_NOTIFIC - DT_SIN_PRI)

# Visualizando a tabela {`dif_tempo`} criada
dif_tempo

# Criando a tabela {`dif_tempo_2`}
dif_tempo_2 <- dt_notific |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NOTIFIC, DT_SIN_PRI) |>
  
  # Utilizando a função `mutate()` para criar a nova coluna "Diferenca"
  mutate(Diferenca = as.integer(DT_NOTIFIC - DT_SIN_PRI))

# Visualizando a tabela {`dif_tempo_2`} criada
dif_tempo_2

# Criando a tabela {`idade`}
idade <- dt_notific |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NASC, DT_SIN_PRI) |>
  
  # Utilizando a função `mutate()` para criar a nova coluna "IDADE_DIAS"
  mutate(IDADE_DIAS = as.integer(DT_SIN_PRI - DT_NASC))

# Visualizando a tabela {`idade`} criada
idade

# Alterando a tabela {`idade`}
idade <- dt_notific |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NASC, DT_SIN_PRI) |>
  
  # Utilizando a função `mutate()` para criar as novas colunas de idade
  mutate(
    IDADE_DIAS = as.integer(DT_SIN_PRI - DT_NASC),
    IDADE_ANOS = floor(IDADE_DIAS / 365.25)
  )

# Visualizando a tabela {`idade`} modificada
idade

year(dt_notific$DT_NOTIFIC)

month(dt_notific$DT_NOTIFIC)

day(dt_notific$DT_NOTIFIC)

wday(dt_notific$DT_NOTIFIC)

wday(dt_notific$DT_NOTIFIC, label = TRUE)

wday(dt_notific$DT_NOTIFIC, label = TRUE, abbr = FALSE)

# Transformando todas as datas da variável `DT_NOTIFIC`
# da tabela {`dt_notific`} em semana epidemiológica 
# com a função `epiweek()`
epiweek(dt_notific$DT_NOTIFIC)

# Alterando a tabela {`dt_notific_2`}
dt_notific_2 <- dt_notific |>
  
  # Utilizando a função `mutate()` para criar novas colunas
  mutate(
    
    # Criando variável ano a partir da data de notificação
    # utilizando a função `year()`
    ano = year(DT_NOTIFIC),  
    
    # Criando variável mes a partir da data de notificação
    # utilizando a função `month()`
    mes = month(DT_NOTIFIC), 
    
    # Criando variável dia a partir da data de notificação
    # utilizando a função `day()`
    dia = day(DT_NOTIFIC),
    
    # Criando variável semana_num a partir da data de notificação
    # utilizando a função `wday()`
    semana_num = wday(DT_NOTIFIC), 
    
    # Criando variável semana_nome a partir da data de notificação
    # utilizando a função `wday()`
    semana_nome = wday(DT_NOTIFIC, label = TRUE),
    
    # Criando variável semana_nome_completo a partir da data de notificação
    # utilizando a função `wday()`
    semana_nome_completo = wday(DT_NOTIFIC, label = TRUE, abbr = FALSE),
    
    # Criando variável semana_epidemiológica a partir da data de notificação
    # utilizando a função `epiweek()`
    semana_epidemiologica = epiweek(DT_NOTIFIC)
  )

# Visualizando a tabela {`dt_notific_2`} alterada
dt_notific_2

# Criando a tabela {`dt_notific_3`}
dt_notific3 <- dt_notific |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NOTIFIC) |>
  
  # Utilizando a função `mutate()` para criar a variável "dia_semana"
  mutate(dia_semana = wday(DT_NOTIFIC))

# Visualizando o objeto criado
dt_notific3

# Criando a tabela {`dt_notific_4`}
dt_notific_4 <- dt_notific |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NOTIFIC) |>
  
  # Utilizando a função `mutate()` para criar as novas variáveis
  mutate(dia_semana = wday(DT_NOTIFIC),
         dif_dia_semana = 7 - dia_semana)

# Visualizando a tabela {`dt_notific_4`} criada
dt_notific_4

# Criando a tabela {`dt_notific_5`}
dt_notific_5 <- dt_notific |>
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NOTIFIC) |>
  
  # Utilizando a função `mutate()` para criar as novas variáveis
  mutate(
    dia_semana = wday(DT_NOTIFIC),
    dif_dia_semana = 7 - dia_semana,
    DT_semana_epi = DT_NOTIFIC + dif_dia_semana
  )

# Visualizando a tabela {`dt_notific_5`} criada
dt_notific_5

# Criando a tabela {`dt_notific_6`}
dt_notific_6 <- dt_notific |>
  
  # Selecionando as variáveis que queremos utilizar com a função `select()`
  select(DT_NOTIFIC) |>
  
  # Utilizando a função `mutate()` para criar a nova coluna
  mutate(DT_semana_epi = DT_NOTIFIC + 7 - wday(DT_NOTIFIC))

# Visualizando o objeto criado
dt_notific_6

# Criando uma sequência de datas, criando dias de uma semana
as.Date("2022-01-01") + 0:7

# Criando uma sequência de datas
seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-02-01"), by = "day")

# Salvando a tabela {base_menor} em seu computador na pasta "Meus documentos"
write_csv(x = base_menor,
          file = "C:/R_Vigilancia/base_menor_curso.csv")

install.packages("writexl");library(writexl)
# Salvando a tabela {base_menor} em seu computador na pasta "Meus documentos"
write_xlsx(x = base_menor,
           path = "C:/R_Vigilancia/base_menor_curso.xlsx")

# carregando os pacotes necessários para análise
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(DescTools)) install.packages("DescTools");library(DescTools)
if(!require(summarytools)) install.packages("summarytools");library(summarytools)
if(!require(gtsummary)) install.packages("gtsummary");library(gtsummary)

dados <- read.dbf('C:/R_Vigilancia/NINDINET.dbf')

glimpse(dados)

dados |>
  
  # filtrando os agravos de dengue (código "A90") e hepatite viral (código "B19")
  # com a função filter()
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # agrupando os agravos (coluna ID_AGRAVO) com a função group_by()
  group_by(ID_AGRAVO) |>
  
  # calculando a frequência de casos com as funcões summarise() e n()
  summarise(total = n())

dados |>
  
  # agrupando com a função group_by() os indivíduos por sexo (coluna CS_SEXO)
  group_by(CS_SEXO) |>
  
  # calculando a frequência de casos com as funcões summarise() e n()
  summarise(total = n())

dados |>
  
  #filtrando os agravos de dengue (código "A90") e hepatite viral (código "B19")
  # com a função filter()
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # agrupando com a função group_by() os agravos (coluna ID_AGRAVO) e indivíduos
  # por sexo (coluna CS_SEXO)
  group_by(ID_AGRAVO, CS_SEXO) |>
  
  # calculando a frequência de casos com as funcões summarise() e n()
  summarise(total = n())

dados |>
  
  #filtrando os agravos de dengue (código "A90") e hepatite viral (código "B19")
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # agrupando com a função group_by() os agravos (coluna ID_AGRAVO) e indivíduos 
  # por sexo (coluna CS_SEXO)
  group_by(ID_AGRAVO, CS_SEXO) |>
  
  # calculando a frequência de casos com as funcões summarise() e n()
  summarise(total = n()) |>
  
  # ordenando a frequência de casos (coluna total) em ordem descrescente com a 
  # função arrange()
  arrange(desc(total))

dados |>
  
  # filtrando os agravos de dengue (código "A90") e hepatite viral (código "B19")
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # utilizando a função mutate() para modificar a coluna ID_AGRAVO, removendo as 
  # categorias (levels) em branco após o filtro usando a função droplevels()
  mutate(ID_AGRAVO = droplevels(ID_AGRAVO)) |>
  
  # contando o total de casos por agravo (coluna ID_AGRAVO) e sexo (coluna CS_SEXO)
  # combinações de categorias com contagem 0 permanecem na tabela pelo uso do 
  # argumento .drop = FALSE
  count(ID_AGRAVO, CS_SEXO, .drop = FALSE)

dados |>
  
  # filtrando os agravos de dengue (código "A90") e hepatite viral (código "B19")
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # agrupando com a função group_by() os agravos (coluna ID_AGRAVO)
  group_by(ID_AGRAVO) |>
  
  # calculando a frequência de casos com as funcões summarise() e n()
  summarise(total = n()) |>
  
  # calculando a proporção de casos ao dividir o total pela soma de valores
  # total pelo uso das funções mutate() e sum()
  mutate(proporcao = total / sum(total))

dados |>
  
  # filtrando os agravos de dengue (código "A90") e hepatite viral (código "B19")
  # com a função filter()
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # agrupando com a função group_by() os agravos (coluna ID_AGRAVO)
  group_by(ID_AGRAVO) |>
  
  # calculando a frequência de casos com as funcões summarise() e n()
  summarise(total = n()) |>
  
  # calculando a proporção de casos ao dividir o total pela soma de valores total
  # pelo uso das funções mutate() e sum()
  mutate(proporcao = total / sum(total)) |>
  
  # criando uma nova coluna (porcentagem) com o uso da função mutate()
  # utilizando a função round() para arredondar em duas casas decimais a
  # porcentagem, após multiplicar a coluna "proporcao" por 100
  mutate(porcentagem = round(proporcao * 100, digits = 2))

dados |>
  
  # filtrando os agravos de dengue (código "A90") e hepatite viral (código "B19")
  # com a função filter()
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # utilizando a função mutate() para modificar a coluna ID_AGRAVO, removendo as
  # categorias (levels) em branco após o filtro usando a função droplevels()
  mutate(ID_AGRAVO = droplevels(ID_AGRAVO)) |>
  
  # agrupando com a função group_by() os agravos (coluna ID_AGRAVO)
  group_by(ID_AGRAVO) |>
  
  # calculando novos dados a partir das colunas agrupadas com o uso da função
  # summarise(), calculando a primeira e última data de notificação com as
  # funções min() e max() na coluna DT_NOTIFIC, respectivamente.
  summarise(primeira_data = min(DT_NOTIFIC),
            ultima_data = max(DT_NOTIFIC))

# criando uma nova tabela (dataframe) chamada {`dados_idade`} que receberá as
# transformações a seguir
dados_idade <- dados |>
  
  # criando uma nova coluna chamada IDADE_ANOS com a função mutate() e, nela,
  # calculando a idade em anos. Primeiro, fazendo a diferença em dias entre
  # data de primeiros sintomas e data de notificação e transformando em
  # número inteiro com a função as.integer() e seguido da divisão por 365.25,
  # e, no final, arredondamento para o menor número inteiro com uso da função 
  # floor()
  mutate(IDADE_ANOS = floor(as.integer(DT_SIN_PRI - DT_NASC) / 365.25))

head(dados_idade |> select(NU_NOTIFIC, DT_SIN_PRI, DT_NASC, IDADE_ANOS))

# criando uma nova tabela (dataframe) chamada {`dados_idade_2`}
dados_idade_2 <- dados |>
  
  # filtrando os agravos de dengue (código "A90") e hepatite viral
  # (código "B19") com a função filter()
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # utilizando a função mutate() para modificar a coluna ID_AGRAVO, removendo as
  # categorias (levels) em branco após o filtro usando a função droplevels()
  mutate(ID_AGRAVO = droplevels(ID_AGRAVO)) |>
  
  # criando uma nova coluna chamada IDADE_ANOS com a função mutate() e, nela,
  # calculando a idade em anos. Primeiro, fazendo a diferença em dias entre
  # data de primeiros  sintomas e data de notificação e transformando em
  # número inteiro com a função as.integer(), seguido da divisão por 365.25,
  # e, no final, arredondamento para o menor número inteiro com uso da função floor()
  mutate(IDADE_ANOS = floor(as.integer(DT_SIN_PRI - DT_NASC) / 365.25)) |>
  
  # agrupando com a função group_by() os agravos (coluna ID_AGRAVO)
  group_by(ID_AGRAVO) |>
  
  # calculando novos dados a partir das colunas agrupadas com o uso da função
  # summarise(), calculando a menor e maior idade com as funções min() e max()
  # na coluna IDADE_ANOS, respectivamente.
  summarise(
    menor_idade = min(IDADE_ANOS, na.rm = TRUE),
    maior_idade = max(IDADE_ANOS, na.rm = TRUE)
  )

# visualizandoa a tabela {`dados_idade_2`}
dados_idade_2

# criando uma nova tabela (dataframe) chamada {`dados_descricao`}
dados_descricao <- dados |>
  
  # filtrando os agravos de dengue (código "A90") e hepatite viral
  # (código "B19") com a função filter()
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # utilizando a função mutate() para modificar a coluna ID_AGRAVO, removendo as
  # categorias (levels) em branco após o filtro usando a função droplevels()
  mutate(ID_AGRAVO = droplevels(ID_AGRAVO)) |>
  
  # criando uma nova coluna chamada IDADE_ANOS com a função mutate() e, nela,
  # calculando a idade em anos. Primeiro, fazendo a diferença em dias entre
  # data de primeiros  sintomas e data de notificação e transformando em
  # número inteiro com a função as.integer(), seguido da divisão por 365.25,
  # e, no final, arredondamento para o menor número inteiro com uso da função 
  # floor()
  mutate(IDADE_ANOS = floor(as.integer(DT_SIN_PRI - DT_NASC) / 365.25)) |>
  
  # agrupando com a função group_by() os agravos (coluna ID_AGRAVO)
  group_by(ID_AGRAVO) |>
  
  # calculando novos dados a partir das colunas agrupadas com o uso da função
  # summarise()
  summarise(
    
    # calculando a média de idade com a função mean() na coluna IDADE_ANOS
    media_idade = mean(IDADE_ANOS, na.rm = TRUE),
    
    # calculando a mediana de idade com a função median() na coluna IDADE_ANOS
    mediana_idade = median(IDADE_ANOS, na.rm = TRUE),
    
    # calculando o quantil 25 da distribuição de idade com a função quantile()
    # na coluna IDADE_ANOS e o argumento prob = 0.25
    quantil_25_idade = quantile(IDADE_ANOS, prob = 0.25, na.rm = TRUE)
  )

# visualizando a tabela {`dados_descricao`}
dados_descricao

dados |>
  
  # filtrando os agravos de dengue (código "A90"), 
  # hepatite viral 
  # (código "B19") e Tuberculose (código "A169")
  filter(ID_AGRAVO %in% c("A90", "B19", "A169")) |>
  
  # utilizando a função mutate() para modificar a 
  # coluna ID_AGRAVO, removendo as
  # categorias (levels) em branco após o filtro usando 
  # a função droplevels()
  mutate(ID_AGRAVO = droplevels(ID_AGRAVO)) |>
  
  # criando uma nova coluna chamada IDADE_ANOS com a 
  # função mutate() e, nela,
  # calculando a idade em anos. Primeiro, fazendo a 
  # diferença em dias entre
  # data de primeiros  sintomas e data de notificação 
  # e transformando em
  # número inteiro com a função as.integer(), seguido 
  # da divisão por 365.25,
  # e, no final, arredondamento para o menor número 
  # inteiro com uso da função floor()
  mutate(IDADE_ANOS = floor(as.integer
                            (DT_SIN_PRI - DT_NASC) / 365.25)) |>
  
  # agrupando com a função group_by() os agravos 
  # (coluna ID_AGRAVO)
  group_by(ID_AGRAVO) |>
  
  # calculando novos dados a partir das colunas 
  # agrupadas com o uso da função 
  # summarise()
  summarise(
    
    # calculando a moda da idade com a função Mode() 
    # na coluna IDADE_ANOS
    moda_idade = Mode(IDADE_ANOS, na.rm = TRUE),
    
    # calculando a moda da categoria sexo com a função 
    # Mode() na coluna CS_SEXO
    moda_sexo = Mode(CS_SEXO, na.rm = TRUE)
  )

# criando uma nova tabela (dataframe) chamada {`dados_var`} a partir da tabela 
# {`dados`}
dados_var <- dados |>
  
  # filtrando os agravos de dengue (código "A90") e hepatite viral
  # (código "B19") com a função filter()
  filter(ID_AGRAVO %in% c("A90", "B19")) |>
  
  # utilizando a função mutate() para modificar a coluna ID_AGRAVO, removendo as
  # categorias (levels) em branco após o filtro usando a função droplevels()
  mutate(ID_AGRAVO = droplevels(ID_AGRAVO)) |>
  
  # criando uma nova coluna chamada IDADE_ANOS com a função mutate() e, nela,
  # calculando a idade em anos. Primeiro, fazendo a diferença em dias entre
  # data de primeiros  sintomas e data de notificação e transformando em
  # número inteiro com a função as.integer(), seguido da divisão por 365.25,
  # e, no final, arredondamento para o menor número inteiro com uso da função 
  # floor()
  mutate(IDADE_ANOS = floor(as.integer(DT_SIN_PRI - DT_NASC) / 365.25)) |>
  
  # agrupando com a função group_by() os agravos (coluna ID_AGRAVO)
  group_by(ID_AGRAVO) |>
  
  # calculando novos dados a partir das colunas agrupadas com o uso da função
  # summarise()
  summarise(
    
    # calculando a variância da idade com a função var() na coluna IDADE_ANOS
    variancia_idade = var(IDADE_ANOS, na.rm = TRUE),
    
    # calculando o desvio padrão da idade com a função sd() na coluna IDADE_ANOS
    desvio_padrao_idade = sd(IDADE_ANOS, na.rm = TRUE)
  )

# visualizando a tabela {`dados_var`}
dados_var

# 1º exemplo: sem utilização de filtro pelo agravo
dados |>
  
  # criando uma nova coluna chamada IDADE_ANOS com a função mutate() e, nela,
  # calculando a idade em anos. Primeiro, fazendo a diferença em dias entre
  # data de primeiros  sintomas e data de notificação e transformando em
  # número inteiro com a função as.integer(), seguido da divisão por 365.25,
  # e, no final, arredondamento para o menor número inteiro com uso da função
  # floor()
  mutate(IDADE_ANOS = floor(as.integer(DT_SIN_PRI - DT_NASC) / 365.25)) |>
  
  # selecionando as colunas ID_AGRAVO, DT_NOTIFIC, DT_SIN_PRI, IDADE_ANOS
  # e CS_SEXO
  select(ID_AGRAVO, DT_NOTIFIC, DT_SIN_PRI, IDADE_ANOS, CS_SEXO) |>
  
  # sumarizando o banco utilizando a função summary()
  summary()

# 2º exemplo: filtro pelo agravo dengue ("A90")
dados |>
  
  # filtrando os agravos de dengue (código "A90") com a função filter()
  filter(ID_AGRAVO == "A90") |>
  
  # utilizando a função mutate() para modificar a coluna ID_AGRAVO, removendo as
  # categorias (levels) em branco após o filtro usando a função droplevels()
  mutate(ID_AGRAVO = droplevels(ID_AGRAVO)) |>
  
  # criando uma nova coluna chamada IDADE_ANOS com a função mutate() e, nela,
  # calculando a idade em anos. Primeiro, fazendo a diferença em dias entre
  # data de primeiros  sintomas e data de notificação e transformando em
  # número inteiro com a função as.integer(), seguido da divisão por 365.25,
  # e, no final, arredondamento para o menor número inteiro com uso da função 
  # floor()
  mutate(IDADE_ANOS = floor(as.integer(DT_SIN_PRI - DT_NASC) / 365.25)) |>
  
  # selecionando as colunas ID_AGRAVO, DT_NOTIFIC, DT_SIN_PRI, IDADE_ANOS
  # e CS_SEXO
  select(ID_AGRAVO, DT_NOTIFIC, DT_SIN_PRI, IDADE_ANOS, CS_SEXO) |>
  
  # sumarizando o banco utilizando a função summary()
  summary()

# 3º exemplo: filtro pelo agravo hepatite viral ("B19")
dados |>
  
  # filtrando os agravos de hepatite viral (código "B19") com a função filter()
  filter(ID_AGRAVO == "B19") |>
  
  # utilizando a função mutate() para modificar a coluna ID_AGRAVO, removendo as
  # categorias (levels) em branco após o filtro usando a função droplevels()
  mutate(ID_AGRAVO = droplevels(ID_AGRAVO)) |>
  
  # criando uma nova coluna chamada IDADE_ANOS com a função mutate() e, nela,
  # calculando a idade em anos. Primeiro, fazendo a diferença em dias entre
  # data de primeiros  sintomas e data de notificação e transformando em
  # número inteiro com a função as.integer(), seguido da divisão por 365.25,
  # e, no final, arredondamento para o menor número inteiro com uso da função 
  # floor()
  mutate(IDADE_ANOS = floor(as.integer(DT_SIN_PRI - DT_NASC) / 365.25)) |>
  
  # selecionando as colunas ID_AGRAVO, DT_NOTIFIC, DT_SIN_PRI, IDADE_ANOS
  # e CS_SEXO
  select(ID_AGRAVO, DT_NOTIFIC, DT_SIN_PRI, IDADE_ANOS, CS_SEXO) |>
  
  # sumarizando o banco utilizando a função summary()
  summary()

dados |>
  
  # criando uma nova coluna chamada IDADE_ANOS com a função mutate() e, nela,
  # calculando a idade em anos. Primeiro, fazendo a diferença em dias entre
  # data de primeiros  sintomas e data de notificação e transformando em
  # número inteiro com a função as.integer(), seguido da divisão por 365.25,
  # e, no final, arredondamento para o menor número inteiro com uso da função
  # floor()
  mutate(IDADE_ANOS = floor(as.integer(DT_SIN_PRI - DT_NASC) / 365.25)) |>
  
  # selecionando as colunas ID_AGRAVO, DT_NOTIFIC, DT_SIN_PRI, IDADE_ANOS
  # e CS_SEXO
  select(ID_AGRAVO, DT_NOTIFIC, DT_SIN_PRI, IDADE_ANOS, CS_SEXO) |>
  
  # criando o sumário com a função dfSummary()
  dfSummary() |> 
  
  # visualizando o relatório em formato HTML com função view()
  view()

# criando objeto do tipo dataframe (tabela) {`Dados`} com o banco de dados 
# {`NINDINET.dbf`}
dados <- read.dbf('C:/R_Vigilancia/NINDINET.dbf')

# Criando um novo objeto a partir da tabela {`Dados`}
tabela_inc_hepatite <- dados |>
  
  # Utilizando a função filter() para diversos critérios de filtragem de dados
  filter(
    
    # Filtrando os agravos de hepatites virais (código "B19")
    ID_AGRAVO == "B19",
    
    # Filtrando apenas casos com ano de primeiros sintomas igual a 2012
    year(DT_SIN_PRI) == 2012) |>
  
  # Removendo as categorias (levels) ausentes na coluna ID_AGRAVO filtrada pelo
  # uso da função droplevels()
  mutate(ID_AGRAVO = droplevels(ID_AGRAVO)) |>
  
  # Agrupando por município de residência
  group_by(ID_MN_RESI) |>
  
  # Contanto o número de casos novos por município de residência
  summarise(N_CASOS_NOVOS = n())

# Visualizando a tabela resultante
tabela_inc_hepatite

# Criando a tabela "tabela_populacao" (dataframe) a partir do arquivo 
# "tabela.csv", que contém o tamanho populacional por município de Rosas
tabela_populacao <- read_csv2("C:/R_Vigilancia/tabela.csv")

# Visualizando a tabela carregada 
tabela_populacao

# Unindo a tabela "tabela_inc_hepatite" com "tabela_populacao", pela coluna com
# o código do município em cada uma
tabela_inc_hepatite |>
  
  # unindo a tabala de casos de hepatites virais com a tabela de população
  # utilizando os códigos do municípios de cada tabela
  left_join(tabela_populacao, by = c("ID_MN_RESI" = "cod_mun")) |>
  
  # Criando uma nova coluna com a incidência de casos por 100000 habitantes com
  # a função mutate()
  mutate(INCIDENCIA = N_CASOS_NOVOS / POPULACAO * 100000)

# criando objeto do tipo data.frame {`base_hans`} que armazenará o banco
# de dados de hanseníase do Estado do Acre
base_hans <- read.dbf(file = 'C:/R_Vigilancia/base_hans_ac.dbf')

base_hans |>
  
  # Criando uma nova coluna com o ano de diagnóstico com a função mutate() e year()
  mutate(ano_diag = year(DT_DIAG)) |>
  
  # Filtrando os casos diagnosticados entre 2019 e 2021 e "tipo de saída" 
  # (TPALTA_N) com valores em branco
  filter(ano_diag >= 2019, ano_diag <= 2021, is.na(TPALTA_N)) |>
  
  # Contanto o número de casos por ano de diagnóstico com a função count()
  count()

# Criando o objeto {`pop_ac`} com a população estimada para o Acre
# O argumento `col_types = list("character")` indica que todas as 
# colunas serão lidas como strings de texto (character)
pop_ac <- read_csv2('C:/R_Vigilancia/pop_ac_09_21.csv', col_types = list("character"))

# Visualizando as linhas e colunas do dataframe {`pop_ac`} 
glimpse(pop_ac)

pop_ac |>
  
  # Criando a coluna com a população total do Estado do Acre em 
  # 2021 com a função summarise() e sum()
  summarise(pop_total_ac_2021 = sum(`2021`))

# Divisão do número de casos pela população do Acre, seguida da multiplicação com 
# 10000
(171 / 906876) * 10000

# Criando uma tabela (dataframe) com o nome casos_hans_ac_21
casos_hans_ac_21 <- base_hans |>
  
  # Filtrando os casos na coluna "tipo de saída" (TPALTA_N) com valores em branco
  filter(is.na(TPALTA_N)) |>
  
  # Contando o número de casos com valores em branco por município
  count(MUNIRESAT)

# Visualizando a tabela resultante
casos_hans_ac_21

# Criando uma tabela (dataframe) com o nome {`prev_casos_ac`}
prev_casos_ac <- casos_hans_ac_21 |>
  
  # Juntando a tabela casos_hans_ac_21 com pop_ac, pelas colunas com os códigos de 
  # município de cada uma
  left_join(pop_ac, by = c("MUNIRESAT" = "Codigo"))

# Visualizando a tabela resultante
prev_casos_ac

prev_casos_ac |>
  
  # Criando uma coluna com o cálculo de prevalência da população por 10 mil
  # habitantes em 2021 com a função mutate()
  mutate(prevalencia_2021 = (n / `2021`) * 10000) |>
  
  # Selecionando apenas as colunas MUNIRESAT, Municipio e prevalencia_2021 com
  # a função select()
  select(MUNIRESAT, Municipio, prevalencia_2021)

# Importando diferentes banco de dados para o R
base_obito_ac <- read.dbf(file = 'C:/R_Vigilancia/do_ac.dbf')
grupos_causas <- read_csv2(file = 'C:/R_Vigilancia/CID-10-GRUPOS.csv')
pop_ac <- read_csv2(file = 'C:/R_Vigilancia/pop_ac_09_21.csv', col_types = list("character"))
pop_ac_sexo_idade <- read_csv2(file = 'C:/R_Vigilancia/pop_ac_sexo_idade_20.csv')
nv_ac <- read_csv2(file = 'C:/R_Vigilancia/nv_ac_15_20.csv')

# Criando uma tabela (dataframe) com o nome base_obito_ac
base_obito_ac <- base_obito_ac |>
  
  # Criando novas colunas com a função mutate()
  mutate(
    
    # Criando coluna com extração de apenas os 3 primeiros caracteres
    # da coluna CAUSABAS pelo uso da função str_sub()
    CAUSA_BASICA_3C = str_sub(CAUSABAS, 1, 3),
    
    # Convertendo as informações na coluna DTOBITO para o formato de
    # data (DD-MM-YYYY) com a função dmy()
    DTOBITO = dmy(DTOBITO),
    
    # Obtendo apenas o ano de óbito a partir da coluna DTOBITO com a função year()
    ANO_OBITO = year(DTOBITO),
    
    # Convertendo as informações na coluna DTNASC para o formato de
    # data (DD-MM-YYYY) com a função dmy()
    DTNASC = dmy(DTNASC),
    
    # Transformando os códigos de sexo nos nomes correspondentes com a 
    # função case_when()
    SEXO = case_when(
      SEXO == "1" ~ "masculino",
      SEXO == "2" ~ "feminino",
      TRUE ~ NA_character_
    ),
    
    # Convertendo a coluna SEXO em uma variável do tipo factor
    SEXO = factor(SEXO, levels = c("masculino", "feminino")),
    
    # Transformando os códigos de raça/cor nos nomes correspondentes com a 
    # função case_when()
    RACACOR = case_when(
      RACACOR == "1" ~ "Branca",
      RACACOR == "2" ~ "Preta",
      RACACOR == "3" ~ "Amarela",
      RACACOR == "4" ~ "Parda",
      RACACOR == "5" ~ "Indígena",
      TRUE ~ NA_character_
    ),
    
    # Transformando os códigos de unidade da idade nos nomes correspondentes com a
    # função case_when(). A classificação utiliza apenas o primeiro caractere da
    # coluna IDADE, com uso da função str_sub()
    UNIDADE_IDADE = case_when(
      (str_sub(IDADE, 1, 1) == "5") ~ "anos+",
      (str_sub(IDADE, 1, 1) == "4") ~ "anos",
      (str_sub(IDADE, 1, 1) == "3") ~ "meses",
      (str_sub(IDADE, 1, 1) == "2") ~ "dias",
      (str_sub(IDADE, 1, 1) == "1") ~ "horas",
      (str_sub(IDADE, 1, 1) == "0") ~ "minutos"
    ),
    
    # Extraindo os dois últimos dígitos da idade com a função str_sub() e
    #  convertendo para número utilizando a função as.numeric()
    IDADE_NUM = as.numeric(str_sub(IDADE, 2, 3)),
    
    # Criando uma classificação de faixa etária por intervalos utilizando
    # as variáveis criadas acima
    FX_ETARIA_11C = case_when(
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 0, 4)) ~ "0 a 4",
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 5, 9)) ~ "5 a 9",
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 10, 14)) ~ "10 a 14",
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 15, 19)) ~ "15 a 19",
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 20, 29)) ~ "20 a 29",
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 30, 39)) ~ "30 a 39",
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 40, 49)) ~ "40 a 49",
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 50, 59)) ~ "50 a 59",
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 60, 69)) ~ "60 a 69",
      (UNIDADE_IDADE == "anos" & between(IDADE_NUM, 70, 79)) ~ "70 a 79",
      ((UNIDADE_IDADE == "anos" & IDADE_NUM >= 80) | UNIDADE_IDADE == "anos+") ~ 
        "80 e mais",
      TRUE ~ NA_character_
    ),
    # Convertendo a coluna FX_ETARIA_11C em uma variável do tipo factor
    FX_ETARIA_11C = factor(
      FX_ETARIA_11C,
      levels = c(
        "0 a 4"  , "5 a 9"  , "10 a 14", "15 a 19", "20 a 29", "30 a 39", 
        "40 a 49", "50 a 59", "60 a 69", "70 a 79", "80 e mais"
      )
    ),
    
    # Reclassificando variáveis da coluna FX_ETARIA_M1ANO para os casos em que 
    # a idade seja menor do que 1 ano
    FX_ETARIA_M1ANO = case_when(
      (UNIDADE_IDADE == "anos" & IDADE_NUM < 1) ~ "< 1 ano",
      (UNIDADE_IDADE == "dias") ~ "< 1 ano",
      (UNIDADE_IDADE == "meses") ~ "< 1 ano",
      (UNIDADE_IDADE == "horas") ~ "< 1 ano",
      (UNIDADE_IDADE == "minutos") ~ "< 1 ano",
      TRUE ~ NA_character_
    ),
    
    # Transformando os códigos de local de ocorrência nos nomes correspondentes 
    # com a função case_when()
    LOCOCOR = case_when(
      LOCOCOR == "1" ~ "Hospital",
      LOCOCOR == "2" ~ "Outros estab saude",
      LOCOCOR == "3" ~ "Domicilio",
      LOCOCOR == "4" ~ "Via publica",
      LOCOCOR == "5" ~ "Outros",
      TRUE ~ NA_character_
    )
  ) |>
  # Unindo a tabela resultante com a tabela grupos_causas pelas colunas com os
  #  códigos de causa básica em cada uma
  left_join(grupos_causas, by = c("CAUSA_BASICA_3C" = "CAT"))

# Criando uma tabela (dataframe) com o nome ob_geral_ac_15_20
ob_geral_ac_15_20 <- base_obito_ac |>
  
  # Contando o número de casos por ano de óbito (ANO_OBITO) com o uso 
  # da função count()
  count(ANO_OBITO)

# Visualizando a tabela resultante
ob_geral_ac_15_20

# Criando uma tabela (dataframe) com o nome pop_ac_15_20
pop_ac_15_20 <- pop_ac |>
  
  # Calculando o número de casos por ano com a função summarise()
  summarise(
    pop_2015 = sum(`2015`),
    pop_2016 = sum(`2016`),
    pop_2017 = sum(`2017`),
    pop_2018 = sum(`2018`),
    pop_2019 = sum(`2019`),
    pop_2020 = sum(`2020`)
  ) |>
  
  # Transformando os dados do formato largo para o formato longo
  pivot_longer(cols = pop_2015:pop_2020,
               values_to = "pop_ac")

# Visualizando a tabela resultante
pop_ac_15_20

# Unindo as duas tabelas com a função bind_cols()
bind_cols(ob_geral_ac_15_20, pop_ac_15_20)

# Unindo as duas tabelas com a função bind_cols()
bind_cols(ob_geral_ac_15_20, pop_ac_15_20) |>
  
  # Criando uma nova coluna com a taxa geral de mortalidade com a função mutate()
  mutate(tx_bruta_ac = (n / pop_ac) * 1000)

base_obito_ac |>
  
  # Filtrando registros utilizando a função filter()
  filter(
    
    #Filtrando os dados por óbitos ocorridos em 2020 com a
    ANO_OBITO == 2020,
    
    # Filtrando os dados e removendo aqueles com valores ausentes para a faixa
    # etária
    !(is.na(FX_ETARIA_11C)),
    
    # Filtrando os dados e removendo aqueles com valores ausentes para sexo
    !(is.na(SEXO)),
    
    # Filtrando os dados por óbitos ocorridos por "Causas externas"
    GRUPOS == "Causas externas") |>
  
  # Contando o número de casos por faixa etária e sexo com a função count()
  count(FX_ETARIA_11C, SEXO) |>
  
  # Transformando a tabela do formato longo para o formato largo com a função 
  # pivot_wider()
  pivot_wider(names_from = SEXO,
              values_from = n) |>
  
  # Unindo a tabela resultante com a tabela pop_ac_sexo_idade com a função 
  # bind_cols()
  bind_cols(pop_ac_sexo_idade) |>
  
  # Criando colunas de taxa específica para homens e mulheres com a função mutate()
  mutate(
    taxa_especifica_homem = masculino / Pop_Masculino * 100000,
    taxa_especifica_mulher = feminino / Pop_Feminino * 100000
  ) |>
  
  # Selecionando apenas as colunas Faixa Etaria 1, taxa_especifica_homem e 
  # taxa_especifica_mulher com a função select()
  select("Faixa Etaria 1",
         taxa_especifica_homem,
         taxa_especifica_mulher)

base_obito_ac |>
  
  # Filtrando os dados por óbitos ocorridos em 2020 com a função filter()
  filter(ANO_OBITO == 2020) |>
  
  # Contando o número de casos por grupos com a função count()
  count(GRUPOS) |>
  
  # Calculando o número de mortes proporcionais em cada grupo pelo número
  #  total com a função mutate()
  mutate(mort_proporcional = (n / sum(n)) * 100)

base_obito_ac |>
  
  # Filtrando os dados por registros de casos com menos de 1 ano com a função
  # filter()
  filter(FX_ETARIA_M1ANO == "< 1 ano") |>
  
  # Contando o número de casos por ano de óbito com a função count()
  count(ANO_OBITO) |>
  
  # Unindo a tabela resultante com a tabela nv_ac com a função bind_cols()
  bind_cols(nv_ac) |>
  
  # Calculando a taxa de mortalidade infantil com a função mutate()
  mutate(tx_mortalidade_infantil = (n / n_nascidos_vivos) * 1000)

base_obito_ac |>
  
  # Utilizando a função filter() para diversos critérios de filtragem de dados
  filter(
    
    # Filtrando os dados por registros de casos do sexo feminino
    SEXO == "feminino",
    
    # Filtrando os registros com casos nas faixas etárias entre 10 e 49 anos
    FX_ETARIA_11C %in% c("10 a 14", "15 a 19",
                         "20 a 29", "30 a 39",
                         "40 a 49"),
    
    # Filtrando os registros de óbitos por causas maternas
    GRUPOS == "Causas Maternas"
  ) |>
  
  # Contando o número de casos por ano de óbito com a função count()
  count(ANO_OBITO) |>
  
  # Unindo a tabela resultante com a tabela nv_ac com a função bind_cols()
  bind_cols(nv_ac) |>
  
  # Calculando a taxa de mortalidade materna com a função mutate()
  mutate(razao_mort_materna = (n / n_nascidos_vivos) * 100000)

# Criando um novo dataframe com a tabela {`Dados`}
dados_classificados <- dados |>
  
  # Filtrando os agravos de dengue (código "A90") com a função filter()
  filter(ID_AGRAVO == "A90") |>
  
  # Criando novas colunas com a função mutate()
  mutate(
    
    # Classificando o município de residência do paciente em Prímula (código 610213)
    # como "Capital" e os demais como "Interior" com uso da função if_else().
    # Ou seja, se o município for igual a 610213 será classificado como Capital e, 
    # se não, será
    # classificado como Interior. Caso houver registros em branco (NA), 
    #estes continuarão como (NA)
    regiao = if_else(ID_MN_RESI == "610213", "Capital", "Interior", NA_character_),
    
    # Classificando os casos em diagnósticos de "casos prováveis" e "descartado"
    # com a função case_when()
    classificacao = case_when(
      CLASSI_FIN %in% c("1", "2", "3", "4", "8") ~ "Casos prováveis",
      CLASSI_FIN == "5" ~ "Casos descartados",
      TRUE ~ NA_character_
    )
  ) 

# Criando uma tabela no formato do pacote gtsummary com o nome tabela
tabela_resid_diag_dengue <- dados_classificados |>
  
  # Criando uma tabela de resumo do cruzamento de informações sobre região e tipo 
  # de diagnóstico
  tbl_cross(
    row = regiao,
    col = classificacao,
    percent = "row",
    missing = "no"
  )

# visualizando a tabela "tabela"
tabela_resid_diag_dengue

# Adicionado o teste estatítico com a função add_p()
tabela_resid_diag_dengue |> add_p()

#Inserindo os dados para construir a tabela (dataframe) 
#2x2
#Capital = c(casos prováveis, casos descartados),
#Interior = c(casos prováveis, casos descartados)
data.frame(Capital = c(11672, 442),
           Interior = c(314, 13)) |> 
  #calculando agora o teste qui-quadrado
  chisq.test()

# Inserindo os dados para construir a tabela (dataframe) 
# 2x2
data.frame(Capital = c(3, 2),
           Interior = c(2, 3)) |> 
  
  # O teste exato de Fisher é feito usando a função 
  # `fisher.test()`
  fisher.test(dataframe,
              # argumento `alternative`indica a hipótese 
              # alternativa do teste
              alternative = "greater", 
              # argumento `conf.int` indica que o 
              # intervalo de confiança deve ser construído
              conf.int = TRUE,
              # argumento `conf.level` indica o nível de 
              # confiança a ser utilizado para a 
              # construção do intervalo
              conf.level= 0.95
  )

# Carregando o banco de dados do surto e criando uma tabela (dataframe) com o nome 
# base_surto
base_surto_18_04 <- read_csv("C:/R_Vigilancia/base_surto.csv")

# Utilizando a função theme_gtsummary_compact() para facilitar a visualização da 
# tabela que será gerada
theme_gtsummary_compact()

base_surto_18_04 |>
  
  # Removendo as colunas id, data_inicio_sintomas, hora_inicio_sintomas e 
  # hora_jantar com a função select()
  select(-id,
         -data_inicio_sintomas,
         -hora_inicio_sintomas,
         -hora_jantar) |>
  
  # Criando uma tabela com resumo das informações de acordo com a coluna "doente"
  tbl_summary(by = doente, missing = "no") |>
  
  # Adicionado o p-valor com a função add_p()
  add_p()

install.packages("epitools")
library(epitools)

resultado_rr_surto <- riskratio(
  x = c(43, 11, 3, 18),
  method = "wald",
  rev = "both"
)

# Visualizando os nomes do objeto resultado_rr_surto
resultado_rr_surto

# Selecionando o objeto "measure" da lista resultado_rr_surto
resultado_rr_surto[["measure"]]

library(foreign)
library(dplyr)


# Importando os dados {`base_tb_ac.dbf`} e armazenando 
# no objeto (dataframe) de nome {`base_tb`}
base_tb <- read.dbf(file = 'C:/R_Vigilancia/base_tb_ac.dbf')

# Selecionando apenas as colunas HIV e SITUA_ENCE
base_tb <- base_tb |> select(HIV, SITUA_ENCE)

# Atualizando o objeto base_tb com as alterações que serão feitas
base_tb <- base_tb |>
  
  # Utilizando a função mutate() para criar novas colunas
  mutate(
    
    # Transformando os códigos de situação de encerramento nos nomes correspondentes 
    #com a função case_when()
    SITUA_ENCE = case_when(
      SITUA_ENCE == "1" ~ "Favorável",
      SITUA_ENCE %in% c("2", "3", "7", "9", "10") ~ "Desfavorável",
      TRUE ~ NA_character_
    ),
    
    # Transformando os códigos de resultado de HIV nos nomes correspondentes com a 
    # função case_when()
    HIV = case_when(HIV == "1" ~ "Positivo",
                    HIV == "2" ~ "Negativo",
                    TRUE ~ NA_character_),
    
    # Convertendo as colunas SITUA_ENCE e HIV em variáveis do tipo factor
    SITUA_ENCE = factor(SITUA_ENCE, levels = c("Desfavorável", "Favorável")),
    HIV = factor(HIV, levels = c("Positivo", "Negativo"))
  )

library(gtsummary)


base_tb |>
  
  # Criando uma tabela de resumo do cruzamento de informações sobre
  # resultado para HIV e situação de encerramento
  tbl_cross(
    row = HIV,
    col = SITUA_ENCE,
    percent = "row",
    missing = "no"
  ) |>
  
  # Adicionado o p-valor com a função add_p()
  add_p()

# Calculando o oddsratio com a função oddsratio()
resultado_or_tb <- oddsratio(x = c(16, 92, 251, 3907),
                             method = "wald",
                             rev = "both")

# Selecionando o objeto "measure" da lista resultado_or_tb
resultado_or_tb[["measure"]]
