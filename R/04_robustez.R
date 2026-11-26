# Verificações adicionais, fora do relatório principal. Servem para checar se
# as conclusões dependem de escolhas de método e sustentam as ressalvas do texto.

titulo("R1 – Mudança de intenção só entre quem tinha intenção definida nos dois momentos")
ns_ing <- d$intencao_ingresso == "Não sabe"; ns_atu <- d$intencao_atual == "Não sabe"
cat("Dos", sum(d$mudou), "que 'mudaram':", sum(ns_ing & !ns_atu), "não sabiam no ingresso e definiram;",
    sum(!ns_ing & ns_atu), "passaram a não saber\n")
amb <- d[!ns_ing & !ns_atu, ]
res$r1_ic_mudou_definidos <- ic_prop(sum(amb$mudou), nrow(amb))
mostra(sprintf("Mudaram de área (%d de %d)", sum(amb$mudou), nrow(amb)), as.list(res$r1_ic_mudou_definidos))
res$r1_turma_mudou <- qui_quadrado(table(d$ingresso, d$mudou))
cat("Mudança por turma (%):", round(100 * tapply(d$mudou, d$ingresso, mean), 1), "\n")
mostra("Turma × mudou", res$r1_turma_mudou)

titulo("R2 – Ponderação pelos estratos (partilha igual, P1-1 slide 27)")
# Supõe turmas de ingresso de tamanho semelhante (pesos iguais, 1/3 cada).
ph <- tapply(def$desvio, def$ingresso, mean); nh <- table(def$ingresso); W <- rep(1 / 3, 3)
p_pond <- sum(W * ph); se_pond <- sqrt(sum(W^2 * ph * (1 - ph) / nh)); z <- qnorm(0.975)
res$r2_desvio_ponderado <- c(p = p_pond, inf = p_pond - z * se_pond, sup = p_pond + z * se_pond)
mostra("Desvio ponderado (pesos iguais)", as.list(res$r2_desvio_ponderado))
cat("Não ponderado:", fmt(mean(def$desvio), 4), "\n")

titulo("R3 – Q3 pelo teste de McNemar")
mc <- mcnemar.test(matrix(c(0, res$q3_voltou, res$q3_saiu, 0), 2), correct = FALSE)
cat(sprintf("Q = (b − c)²/(b + c) = %s | Z² = %s | valor-p = %s\n",
            fmt(unname(mc$statistic), 3), fmt(res$q3_teste$z^2, 3), format.pval(mc$p.value, 3)))
cat("Teste binomial exato: valor-p =", format.pval(binom.test(res$q3_saiu, res$q3_saiu + res$q3_voltou)$p.value, 3), "\n")

titulo("R4 – Q6 com outros métodos")
mostra("Bartlett com correção C (original)", list(D = res$q6_bartlett_original$D_com_correcao))
mostra("Bartlett com correção C (ln)", list(D = res$q6_bartlett_ln$D_com_correcao))
av_o <- anova_1f(d$salario, d$intencao5)
mostra("ANOVA na escala original", av_o[c("F", "Fc", "valor_p")])
kw <- kruskal.test(d$salario, d$intencao5)
cat("Kruskal-Wallis: H =", fmt(unname(kw$statistic), 3), "| valor-p =", format.pval(kw$p.value, 3), "\n")
sw <- shapiro.test(res$q6_anova$residuos)
cat("Shapiro-Wilk nos resíduos de ln(salário): W =", fmt(unname(sw$statistic), 4),
    "| valor-p =", fmt(sw$p.value, 3), "\n")
cat("Valor crítico aproximado de Lilliefors (parâmetros estimados): 0,886/√n =",
    fmt(0.886 / sqrt(nrow(d)), 4), "contra K =", fmt(res$q6_ks_ln$K, 4), "\n")

titulo("R5 – Definição de desvio sem a categoria 'Outro'")
alt <- def$intencao_atual %in% c("Mercado financeiro", "Consultoria", "Empreendedorismo")
cat(sprintf("Se 'Outro' (pesquisa, setor público) contasse como dentro: %d de %d = %.1f%%\n",
            sum(alt), nrow(def), 100 * mean(alt)))

titulo("R6 – Regressão linear simples: ln(salário) em função dos processos seletivos")
m <- lm(ln_salario ~ processos, data = d); s <- summary(m)
cat(sprintf("b0 = %s | b1 = %s | R² = %s | F = %s | valor-p = %s\n",
            fmt(coef(m)[1], 4), fmt(coef(m)[2], 4), fmt(s$r.squared, 3),
            fmt(s$fstatistic[1], 3), format.pval(pf(s$fstatistic[1], 1, s$fstatistic[3], lower.tail = FALSE), 3)))
cat("Atenção: 56% dos estudantes têm zero processos e o máximo é 30; pontos extremos pesam no ajuste.\n")
