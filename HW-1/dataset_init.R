# Primeiro, carregamos o dataset completo para o ambiente do R
df <- read.csv("data/HW1_bike_sharing.csv")

# Em seguida, fazemos o corte desejado que é especificado da seguinte maneira
# r = 1 + (M mod 100) tal que
# M = max(568346, 570305, 571953) = 571953
# Logo, r = 54

# Usaremos as 300 observações dado no seguinte intervalo:
# [r, r+300-1]
data_group <- df[54:353, ]

# Criamos também, para a questão 2, a variável total_user que é dada por
# total_user = casual + registered
data_group$total_user <- data_group$casual + data_group$registered
write.csv(data_group, "data/data_group.csv", row.names = FALSE)


