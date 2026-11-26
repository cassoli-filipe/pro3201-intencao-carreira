# Análise inferencial (seção 4.3 do relatório), α = 5%.
# Os resultados ficam na lista `res`, usada por testes/conferencia.R.

res <- list()

titulo("Tabela 7 – Testes qui-quadrado de associação")
def <- d[!is.na(d$desvio), ]
res$q2_turma_desvio   <- qui_quadrado(table(def$ingresso, def$desvio))
res$q4_turma_certeza  <- qui_quadrado(table(d$ingresso, d$certeza3))
res$q5_curso          <- qui_quadrado(table(d$curso5, d$intencao3))   # sem "Outro curso" (n = 201)
res$q5_entidade       <- qui_quadrado(table(d$entidade, d$intencao3))
res$q5_processos      <- qui_quadrado(table(d$processos3, d$intencao3))
res$q6_media          <- qui_quadrado(table(d$media3, d$intencao3))
for (nm in grep("^q[2-6]_", names(res), value = TRUE)) mostra(nm, res[[nm]])

titulo("Q1 – Proporção que pretende atuar fora de engenharia/tecnologia")
res$q1_ic    <- ic_prop(sum(def$desvio), nrow(def))
res$q1_teste <- teste_prop(sum(def$desvio), nrow(def), p0 = 0.62, alternativa = "menor")
mostra("IC 95%", as.list(res$q1_ic))
mostra("H0: p = 0,62; H1: p < 0,62", res$q1_teste)

titulo("Q3 – Mudança de intenção desde o ingresso")
res$q3_ic_mudou <- ic_prop(sum(d$mudou), nrow(d))
mostra("IC 95% (mudou)", as.list(res$q3_ic_mudou))
ambos <- d[!is.na(d$desvio) & !is.na(d$desvio_ing), ]
saiu  <- sum(ambos$desvio_ing == 0 & ambos$desvio == 1)   # de engenharia/tecnologia para fora
voltou <- sum(ambos$desvio_ing == 1 & ambos$desvio == 0)  # caminho inverso
cat("Definida nos dois momentos:", nrow(ambos), "| saíram:", saiu, "| caminho inverso:", voltou,
    sprintf("| fora: %.1f%% no ingresso, %.1f%% hoje\n",
            100 * mean(ambos$desvio_ing), 100 * mean(ambos$desvio)))
res$q3_n_ambos <- nrow(ambos); res$q3_saiu <- saiu; res$q3_voltou <- voltou
res$q3_teste <- teste_prop(saiu, saiu + voltou, p0 = 0.5, alternativa = "bilateral")
mostra("H0: p = 0,5 (bilateral)", res$q3_teste)

titulo("Q5 – Duas proporções: desvio com e sem entidade")
x <- tapply(def$desvio, def$entidade, sum); n <- table(def$entidade)
res$q5_duas_prop <- teste_duas_prop(x[["Sim"]], n[["Sim"]], x[["Não"]], n[["Não"]])
cat(sprintf("Participantes: %d de %d | Não participantes: %d de %d\n",
            x[["Sim"]], n[["Sim"]], x[["Não"]], n[["Não"]]))
mostra("H0: p1 = p2", res$q5_duas_prop)

titulo("Q6 – Expectativa salarial: Bartlett, KS, ANOVA e Scheffé")
res$q6_bartlett_original <- bartlett_slide(d$salario, d$intencao5)
res$q6_bartlett_ln       <- bartlett_slide(d$ln_salario, d$intencao5)
mostra("Bartlett, escala original", res$q6_bartlett_original)
mostra("Bartlett, ln(salário)", res$q6_bartlett_ln)
av_original <- anova_1f(d$salario, d$intencao5)
res$q6_anova <- anova_1f(d$ln_salario, d$intencao5)
res$q6_ks_original <- ks_residuos(av_original$residuos)
res$q6_ks_ln       <- ks_residuos(res$q6_anova$residuos)
mostra("KS resíduos, escala original", res$q6_ks_original)
mostra("KS resíduos, ln(salário)", res$q6_ks_ln)

cat("\nTabela 8 – ANOVA com um fator para ln(expectativa salarial)\n")
a <- res$q6_anova
print(data.frame(Fonte = c("Entre grupos", "Dentro dos grupos", "Total"),
                 SQ = round(c(a$SQE, a$SQD, a$SQT), 3), gl = a$gl,
                 QM = c(round(c(a$Se2, a$Sp2), 3), NA),
                 F = c(round(a$F, 3), NA, NA), F_critico = c(round(a$Fc, 3), NA, NA),
                 valor_p = c(signif(a$valor_p, 3), NA, NA)), row.names = FALSE)

cat("\nTabela 9 – Comparações múltiplas de Scheffé (ln da expectativa salarial)\n")
res$q6_scheffe <- scheffe(res$q6_anova)
print(transform(res$q6_scheffe, dif_abs = round(dif_abs, 3), delta = round(delta, 3)), row.names = FALSE)

res$q6_media_geometrica <- exp(res$q6_anova$medias)
cat("\nMédia geométrica por grupo (R$):\n"); print(round(res$q6_media_geometrica))
g <- res$q6_media_geometrica
cat(sprintf("Mercado financeiro / Engenharia = %.3f | Mercado financeiro / Outros = %.3f\n",
            g[["Mercado financeiro"]] / g[["Engenharia"]], g[["Mercado financeiro"]] / g[["Outros/Indefinido"]]))
