# Confere cada número publicado no relatório (PDF) com o recalculado em R.
# Um item passa quando a diferença é no máximo meia unidade da última casa
# decimal publicada. Valor-p publicado como "< 0,001" passa se o calculado for menor.
# Termina com erro se algum item divergir.

v <- list()
ok <- function(item, calculado, publicado, casas) {
  v[[length(v) + 1]] <<- data.frame(item = item, calculado = unname(calculado),
                                    publicado = publicado, casas = casas)
}
menor_001 <- function(item, calculado) ok(item, as.numeric(calculado < 0.001), 1, 0)
ch <- function(nome, r, Q, gl, crit, p, E, n = NULL) {
  ok(paste(nome, "Q"), r$Q, Q, 3); ok(paste(nome, "gl"), r$gl, gl, 0)
  ok(paste(nome, "crítico"), r$critico, crit, 3); ok(paste(nome, "menor Eij"), r$menor_E, E, 1)
  if (is.na(p)) menor_001(paste(nome, "valor-p < 0,001"), r$valor_p) else ok(paste(nome, "valor-p"), r$valor_p, p, 3)
  if (!is.null(n)) ok(paste(nome, "n"), r$n, n, 0)
}

# 4.1 e Tabela 4
ok("válidas", nrow(d), 204, 0)
ok("ingresso 2024", sum(d$ingresso == "2024"), 73, 0)
ok("ingresso 2025", sum(d$ingresso == "2025"), 74, 0)
ok("ingresso 2026", sum(d$ingresso == "2026"), 57, 0)
for (cc in names(c(Elétrica = 40, Produção = 38, Civil = 28, Computação = 19, Química = 14, Materiais = 13,
                   Mecânica = 12, Naval = 12, Minas = 9, Ambiental = 7, Mecatrônica = 7, Metalúrgica = 2, Outro = 3)))
  ok(paste("curso", cc), sum(d$curso == cc),
     c(Elétrica = 40, Produção = 38, Civil = 28, Computação = 19, Química = 14, Materiais = 13,
       Mecânica = 12, Naval = 12, Minas = 9, Ambiental = 7, Mecatrônica = 7, Metalúrgica = 2, Outro = 3)[[cc]], 0)

# Tabela 7
ch("Q2 turma × desvio", res$q2_turma_desvio, 1.355, 2, 5.991, 0.508, 24.0, 184)
ch("Q4 turma × certeza", res$q4_turma_certeza, 10.528, 4, 9.488, 0.032, 8.9, 204)
ch("Q5 curso × intenção", res$q5_curso, 31.376, 8, 15.507, NA, 6.2, 201)
ch("Q5 entidade × intenção", res$q5_entidade, 9.604, 2, 5.991, 0.008, 10.0, 204)
ch("Q5 processos × intenção", res$q5_processos, 18.861, 4, 9.488, NA, 8.8, 204)
ch("Q6 média × intenção", res$q6_media, 5.138, 4, 9.488, 0.273, 12.0, 204)

# Q1
ok("Q1 p̂", res$q1_ic[["p"]], 0.489, 3)
ok("Q1 IC inf", res$q1_ic[["inf"]], 0.417, 3); ok("Q1 IC sup", res$q1_ic[["sup"]], 0.561, 3)
ok("Q1 Z", res$q1_teste$z, -3.657, 3); ok("Q1 crítico", res$q1_teste$critico, -1.645, 3)
menor_001("Q1 valor-p < 0,001", res$q1_teste$valor_p)

# Q3
ok("Q3 mudou (%)", 100 * res$q3_ic_mudou[["p"]], 45.6, 1)
ok("Q3 IC inf (%)", 100 * res$q3_ic_mudou[["inf"]], 38.8, 1); ok("Q3 IC sup (%)", 100 * res$q3_ic_mudou[["sup"]], 52.4, 1)
ok("Q3 definidos nos 2 momentos", res$q3_n_ambos, 162, 0)
ok("Q3 saíram", res$q3_saiu, 33, 0); ok("Q3 caminho inverso", res$q3_voltou, 6, 0)
ok("Q3 proporção", res$q3_teste$p, 0.846, 3); ok("Q3 Z", res$q3_teste$z, 4.323, 3)
menor_001("Q3 valor-p < 0,001", res$q3_teste$valor_p)

# Q5 – duas proporções
ok("Q5 p̂ participantes (%)", 100 * res$q5_duas_prop$p1, 56.0, 1)
ok("Q5 p̂ não participantes (%)", 100 * res$q5_duas_prop$p2, 25.6, 1)
ok("Q5 Z", res$q5_duas_prop$z, 3.496, 3)
ok("Q5 IC inf (p.p.)", 100 * res$q5_duas_prop$ic_dif[["inf"]], 15.0, 1)
ok("Q5 IC sup (p.p.)", 100 * res$q5_duas_prop$ic_dif[["sup"]], 45.8, 1)
menor_001("Q5 valor-p < 0,001", res$q5_duas_prop$valor_p)

# Q6 – suposições
ok("Bartlett D original", res$q6_bartlett_original$D, 26.446, 3)
ok("Bartlett crítico", res$q6_bartlett_original$critico, 9.488, 3)
menor_001("Bartlett original valor-p < 0,001", res$q6_bartlett_original$valor_p)
ok("Bartlett D ln", res$q6_bartlett_ln$D, 5.107, 3); ok("Bartlett ln valor-p", res$q6_bartlett_ln$valor_p, 0.276, 3)
ok("KS K original", res$q6_ks_original$K, 0.121, 3); ok("KS K ln", res$q6_ks_ln$K, 0.061, 3)
ok("KS crítico", res$q6_ks_ln$critico, 0.095, 3)

# Tabela 8
a <- res$q6_anova
ok("ANOVA SQE", a$SQE, 3.879, 3); ok("ANOVA SQD", a$SQD, 31.067, 3); ok("ANOVA SQT", a$SQT, 34.946, 3)
ok("ANOVA QM entre", a$Se2, 0.970, 3); ok("ANOVA QM dentro", a$Sp2, 0.156, 3)
ok("ANOVA F", a$F, 6.212, 3); ok("ANOVA F crítico", a$Fc, 2.417, 3)
menor_001("ANOVA valor-p < 0,001", a$valor_p)

# Tabela 9
t9 <- data.frame(dif = c(0.217, 0.315, 0.190, 0.038, 0.097, 0.028, 0.255, 0.125, 0.352, 0.228),
                 delta = c(0.337, 0.237, 0.267, 0.239, 0.362, 0.383, 0.363, 0.298, 0.273, 0.300),
                 diferem = c("Não", "Sim", "Não", "Não", "Não", "Não", "Não", "Não", "Sim", "Não"))
for (i in seq_len(nrow(t9))) {
  ok(paste("Scheffé", res$q6_scheffe$par[i], "|dif|"), res$q6_scheffe$dif_abs[i], t9$dif[i], 3)
  ok(paste("Scheffé", res$q6_scheffe$par[i], "δ"), res$q6_scheffe$delta[i], t9$delta[i], 3)
  ok(paste("Scheffé", res$q6_scheffe$par[i], "decisão"), as.numeric(res$q6_scheffe$diferem[i] == t9$diferem[i]), 1, 0)
}
g <- res$q6_media_geometrica
ok("média geométrica Merc. fin.", g[["Mercado financeiro"]], 8940, 0)
ok("média geométrica Engenharia", g[["Engenharia"]], 6527, 0)
ok("média geométrica Outros", g[["Outros/Indefinido"]], 6286, 0)

# Tabela 6
t6_pub <- rbind(Engenharia = c(78, 6981, 6000, 2825, 3000, 20000),
                Tecnologia = c(16, 8469, 8500, 2642, 5000, 15000),
                `Mercado financeiro` = c(41, 9817, 10000, 5097, 4500, 30000),
                Consultoria = c(29, 8534, 8000, 3171, 2500, 14000),
                `Outros/Indefinido` = c(40, 6888, 7000, 2820, 2000, 13000))
for (gr in rownames(t6_pub)) {
  x <- d$salario[d$intencao5 == gr]
  calc <- c(length(x), mean(x), median(x), sd(x), min(x), max(x))
  for (j in 1:6) ok(paste("Tabela 6", gr, c("n", "média", "mediana", "dp", "mín", "máx")[j]), calc[j], t6_pub[gr, j], 0)
}
ok("salário média", mean(d$salario), 7870, 0); ok("salário dp", sd(d$salario), 3596, 0)
ok("salário assimetria", assimetria_excel(d$salario), 2.17, 2)
ok("processos assimetria", assimetria_excel(d$processos), 3.14, 2)
ok("certeza média", mean(d$certeza), 3.53, 2); ok("certeza dp", sd(d$certeza), 1.07, 2)

conf <- do.call(rbind, v)
conf$confere <- abs(conf$calculado - conf$publicado) <= 0.5 * 10^(-conf$casas) + 1e-9

titulo("Conferência com os valores publicados no relatório")
cat(sprintf("%d de %d números conferem.\n", sum(conf$confere), nrow(conf)))
if (any(!conf$confere)) {
  print(conf[!conf$confere, ], row.names = FALSE)
  stop("Há números do relatório que não conferem com o R.")
}
