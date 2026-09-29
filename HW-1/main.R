for (estacao in unique(data_group$season)) {
  dados <- data_group[data_group$season == estacao, ]
  
  total_users <- dados$total_user
  n_movimentado <- mean(dados$low_usage) * 100
  
  cat("--- Estação:", estacao, "---\n")
  print(c(
    media   = mean(total_users),
    mediana = median(total_users),
    dp      = sd(total_users)
  ))
  
  cat("\nProporção dos dias de 'low_usage' (%):\n")
  print(c(
    "Não Movimentado" = sprintf("%.2f%%", n_movimentado),
    "Movimentado" = sprintf("%.2f%%", 100 - n_movimentado)
  ), quote = FALSE)
  cat("\n\n")
}

# Itera sobre cada estação
for (estacao in unique(data_group$season)) {
  
  # Corte para uma dada estação
  dados_estacao <- data_group[data_group$season == estacao, c("total_user", "low_usage")]
  
  boxplot(dados_estacao$total_user,
          main = paste("Uso de bicicletas no", estacao),
          ylab = "Uso no dia",
          col = "lightblue")
}

for (clima in unique(data_group$weathersit)) {
  dados <- data_group[data_group$weathersit == clima, ]
  
  total_users <- dados$total_user
  n_movimentado <- mean(dados$low_usage) * 100
  
  cat("--- Condições meteriológicas:", clima, "---\n")
  print(c(
    media   = mean(total_users),
    mediana = median(total_users),
    dp      = sd(total_users)
  ))
  
  cat("\nProporção dos dias de 'low_usage' (%):\n")
  print(c(
    "Não Movimentado" = sprintf("%.2f%%", n_movimentado),
    "Movimentado" = sprintf("%.2f%%", 100 - n_movimentado)
  ), quote = FALSE)
  cat("\n\n")
}

# Itera sobre cada estação
for (clima in unique(data_group$weathersit)) {
  
  # Corte para uma dada estação
  dados_clima <- data_group[data_group$weathersit == clima, c("total_user", "low_usage")]
  
  boxplot(dados_clima$total_user,
          main = paste("Uso de bicicletas durante", clima),
          ylab = "Uso no dia",
          col = "lightblue")
}