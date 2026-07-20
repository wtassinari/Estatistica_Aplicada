# Entrando com os dados
salario <- c(12,18,18,12,10,15,11,16,13,14,
             24,22,20,16,18,14,16,13,17,15,
             22,16,24,18,20,19,21,17,18,16,
             12,18,18,12,10,15,17,14,17,15,
             13,14,20,20,18,16,13,15,14,16)

cidades <- c("SP","SP","SP","SP","SP","SP","SP","SP","SP","SP",
             "SB","SB","SB","SB","SB","SB","SB","SB","SB","SB",
             "SC","SC","SC","SC","SC","SC","SC","SC","SC","SC",
             "SA","SA","SA","SA","SA","SA","SA","SA","SA","SA",
             "D","D","D","D","D","D","D","D","D","D")
dados <- data.frame(salario, cidades)

modelo <- aov(salario ~cidades, dados)

summary(modelo)
TukeyHSD(modelo)

correcao = (sum(salario)^2)/50
correcao

SQT = sum(salario^2) - correcao
SQT

SQTr

Tsp = (sum(dados$salario[dados$cidades == 'SP']))
Tsa = (sum(dados$salario[dados$cidades == 'SA']))
Tsb = (sum(dados$salario[dados$cidades == 'SB']))
Tsc = (sum(dados$salario[dados$cidades == 'SC']))
Td = (sum(dados$salario[dados$cidades == 'D']))

SQTr = (Tsp^2 + Tsa^2 + Tsb^2 + Tsc^2 + Td^2)/10 - correcao


Msp = mean(dados$salario[dados$cidades == 'SP'])
Msa = mean(dados$salario[dados$cidades == 'SA'])
Msb =mean(dados$salario[dados$cidades == 'SB'])
Msc = mean(dados$salario[dados$cidades == 'SC'])
Md = mean(dados$salario[dados$cidades == 'D'])
