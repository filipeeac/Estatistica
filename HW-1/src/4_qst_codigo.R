# PREPARAÇÃO SOBRE AS VARIÁVEIS NECESSÁRIAS A OBTER OS RESULTADOS SOLICITADOS NA QUESTÃO 4

# 1. Leitura do dataset do GitHub
url <- "https://raw.githubusercontent.com/filipeeac/Estatistica/refs/heads/main/HW-1/data/HW1_bike_sharing.csv"
dataset <- read.csv(url)

# 2. Operação matemática para encontrar r
r <- 1 + (571953 %% 100)
print(paste("O valor calculado para r é:", r))

# 3. Criação do subconjunto 'data_group'
data_group <- dataset[r:(r + 299), ]

# Visualizando as primeiras linhas do novo dataset para conferência
head(data_group)

# season: 1 - primavera, 2 - verão, 3 - outono, 4 - inverno
# weathersit: 1 - ceu limpo, 2 - nublado, 3 - chuva fraca, 4 - chuva forte

data_group$total_user <- data_group$casual + data_group$registered

# ------------------------------- 4.1 ----------------------------

# 1. Instala o pacote
install.packages("ggplot2")
install.packages("plotly")

# 1. Carregar bibliotecas necessárias
library(ggplot2)
library(plotly)

# 2. Converter a coluna dteday para o formato de Data (Date)
data_group$dteday <- as.Date(data_group$dteday, format = "%Y-%m-%d")

# 3. Criar o gráfico de Série Temporal
plot_ts <- ggplot(data_group, aes(x = dteday, y = total_user)) +
  # Substituição de 'size' por 'linewidth' (padrão nas versões recentes do ggplot2)
  geom_line(color = "gray70", linewidth = 1) +
  geom_point(color = "gray20", size = 2, alpha = 0.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Série Temporal (Total de Usuários)",
    x = "Mês / Ano",
    y = "Quantidade de Uso"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5))

# Exibir o gráfico estático
print(plot_ts)
# ------------------------------- 4.2 ----------------------------
install.packages("ggplot2")
install.packages("dplyr")

# Criação da variável 'low_usage' utilizando a função ifelse
data_group$low_usage <- ifelse(data_group$total_user < Q1_valor, 1, 0)

# 0. Preparação da Variável Categórica
# Transformando low_usage em fator para correta plotagem de cores
data_group$low_usage_factor <- factor(data_group$low_usage, 
                                      levels = c(0, 1), 
                                      labels = c("Normal", "Baixa Utilizacao"))

 # Carregamento dos pacotes necessários
 library(ggplot2)
 library(dplyr)

 # Transformando a variável weathersit em fator para a análise categórica
 data_group_plot <- data_group %>%
   mutate(
     weather_factor = factor(weathersit, levels = c(1, 2, 3, 4),
                             labels = c("Céu Limpo", "Nublado", "Chuva Fraca", "Chuva Forte"))
   )
 
 # VARIÁVEL 1: TEMPERATURA (temp) x TOTAL DE USUÁRIOS (total_user)
 
 # Medida Estatística: Coeficiente de Correlação de Pearson (r)
 cor_temp <- cor(data_group_plot$temp, data_group_plot$total_user, use = "complete.obs")
 cat(sprintf("Medida Estatística (Temperatura) - Correlação de Pearson: r = %.4f\n", cor_temp))
 
 # Visualização: Gráfico de Dispersão com Regressão Linear
 plot_temp <- ggplot(data_group_plot, aes(x = temp, y = total_user)) +
   geom_point(alpha = 0.6, color = "#2c3e50", size = 2) +
   geom_smooth(method = "lm", formula = y ~ x, color = "#e74c3c", se = TRUE) +
   labs(
     title = "Impacto da Temperatura na Demanda",
     subtitle = sprintf("Associação contínua positiva (r = %.4f)", cor_temp),
     x = "Temperatura Normalizada",
     y = "Total de Usuários (Demanda)"
   ) +
   theme_minimal() +
   theme(plot.title = element_text(face = "bold"))
 
 plot(plot_temp)

 # VARIÁVEL 2: CONDIÇÃO METEOROLÓGICA (weathersit) x TOTAL DE USUÁRIOS (total_user)
 
 # Medida Estatística: Média de usuários por categoria climática e a amplitude de queda
 medias_clima <- tapply(data_group_plot$total_user, data_group_plot$weather_factor, mean, na.rm = TRUE)
 queda_limpo_chuva <- medias_clima["Céu Limpo"] - medias_clima["Chuva Fraca"]
 
 cat("\nMedida Estatística (Clima) - Médias por Condição:\n")
 print(medias_clima)
 cat(sprintf("Queda na média (Céu Limpo -> Chuva Fraca): %.2f usuários\n", queda_limpo_chuva))
 
 # Visualização: Boxplot para comparação entre categorias
 plot_weather <- ggplot(data_group_plot, aes(x = weather_factor, y = total_user, fill = weather_factor)) +
   geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.size = 2) +
   stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
   scale_fill_brewer(palette = "Blues", direction = -1) +
   labs(
     title = "Impacto do Clima na Demanda",
     subtitle = "Variação da distribuição sob diferentes condições",
     x = "Condição Meteorológica",
     y = "Total de Usuários (Demanda)"
   ) +
   theme_minimal() +
   theme(legend.position = "none", plot.title = element_text(face = "bold"))
 
 plot(plot_weather)


# ---------------------------------- 4.3 -----------------------------
Q1_valor <- 3093

# Criação da variável 'low_usage' utilizando a função ifelse
data_group$low_usage <- ifelse(data_group$total_user < Q1_valor, 1, 0)

# Conversão para 'factor' (essencial para categorização no gráfico)
data_group$low_usage_factor <- as.factor(data_group$low_usage)

#Inserção de Linhas de Tendência
ggplot(data_group, aes(x = temp, y = total_user, color = low_usage_factor)) +
  geom_point(alpha = 0.5, size = 2) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.2) + # Adiciona a regressão linear
  scale_color_manual(
    values = c("0" = "darkgray", "1" = "red"),
    labels = c("0" = "Normal (>= Q1)", "1" = "Baixa Utilização (< Q1)")
  ) +
  labs(
    title = "Linhas de Tendência (Regressão)",
    subtitle = "Interação entre Temperatura e Diferentes Padrões de Uso",
    x = "Temperatura Normalizada",
    y = "Total de Usuários",
    color = "Status de Utilização"
  ) +
  theme_minimal()


