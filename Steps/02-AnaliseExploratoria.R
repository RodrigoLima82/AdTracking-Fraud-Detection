# Analise Exploratoria de Dados

# Carregando os Pacotes
library(dplyr)

# Verificar se existem valores ausentes (missing) em cada coluna
# Nenhum valor encontrado
any(is.na(df))

# Analisando dados por agrupamentos
df %>% group_by(ip) %>% tally()
df %>% group_by(ip, click_day) %>% tally()

# Adicionando nova coluna no dataframe
df_count_ip <- df %>% 
  count(ip, sort = TRUE, name = "ip_count")

df <- merge(df, df_count_ip, by=c("ip"))
rm('df_count_ip')

df_count_ip_day <- df %>% 
  count(ip, click_day, sort = TRUE, name = "ip_day_count")

df <- merge(df, df_count_ip_day, by=c('ip','click_day'))
rm('df_count_ip_day')

str(df)
View(df)

# Normalizar as variaveis numericas 
cols <- c('ip', 'app', 'device', 'os', 'channel','click_day','click_hour','ip_count', 'ip_day_count') 
df[, cols] <- scale(df[, cols])

# Verificando overfitting dos dados
# 99.773 registros indicam que o app nao foi baixado
# 227 registros indicam que o app foi baixado
table(df$is_attributed)
prop.table(table(df$is_attributed))