# Análise descritiva (seções 4.1 e 4.2 do relatório).

pct <- function(x, d = 1) round(100 * x, d)
tab_pct <- function(t, d = 1) round(100 * prop.table(t, 1), d)

titulo("4.1 Composição da amostra")
print(table(Ingresso = d$ingresso))
print(round(100 * prop.table(table(d$ingresso)), 1))

cat("\nTabela 4 – Distribuição das respostas válidas por curso\n")
t4 <- sort(table(d$curso), decreasing = TRUE)
print(cbind(n = t4, `%` = pct(t4 / nrow(d))))

titulo("4.2 Q1 – Intenção de carreira atual (Figura 1)")
f1 <- table(d$intencao_atual)
print(cbind(n = f1, `%` = pct(f1 / nrow(d))))
def <- d[!is.na(d$desvio), ]
cat("\nCom intenção definida:", nrow(def), "| fora de engenharia/tecnologia:",
    sum(def$desvio), sprintf("(%.1f%%)\n", 100 * mean(def$desvio)))

titulo("Q3 – Intenção no ingresso × atual (Figura 2 e Tabela 5)")
f2 <- rbind(Ingresso = table(d$intencao_ingresso), Atual = table(d$intencao_atual))
print(pct(f2 / nrow(d)))
cat("\nTabela 5 – Matriz de transição (linhas: ingresso; colunas: atual)\n")
t5 <- table(Ingresso = d$intencao_ingresso, Atual = d$intencao_atual)
print(addmargins(t5))
cat("\nMantiveram:", sum(diag(t5)), "| Mudaram:", sum(d$mudou),
    sprintf("(%.1f%%)\n", 100 * mean(d$mudou)))
cat("Mudaram para mercado financeiro ou consultoria:",
    sum(t5[, "Mercado financeiro"]) - t5["Mercado financeiro", "Mercado financeiro"] +
      sum(t5[, "Consultoria"]) - t5["Consultoria", "Consultoria"], "\n")

titulo("Q2 – Desvio pretendido por turma (com intenção definida)")
print(round(100 * tapply(def$desvio, def$ingresso, mean), 1))

titulo("Q4 – Grau de certeza (Figura 3)")
cat("Geral: média", fmt(mean(d$certeza), 2), "| mediana", median(d$certeza),
    "| moda", moda(d$certeza), "| Q1", quartil_slide(d$certeza, .25),
    "| Q3", quartil_slide(d$certeza, .75), "| dp", fmt(sd(d$certeza), 2),
    "| nota 4 ou 5:", sprintf("%.1f%%\n", 100 * mean(d$certeza >= 4)))
print(round(tapply(d$certeza, d$ingresso, mean), 2))
print(tab_pct(table(d$ingresso, d$certeza3)))

titulo("Q5 – Curso, entidade e processos seletivos")
cat("Figura 4 – Intenção (3 grupos) por grupo de curso, % na linha\n")
print(tab_pct(table(d$curso5, d$intencao3), 0))
cat("\nDesvio pretendido por grupo de curso (%):\n")
print(round(100 * tapply(def$desvio, def$curso5, mean), 1))

cat("\nParticipam de entidade:", sum(d$entidade == "Sim"),
    sprintf("(%.1f%%)\n", 100 * mean(d$entidade == "Sim")))
print(tab_pct(table(d$entidade, d$intencao3)))
cat("Desvio pretendido por entidade (%):\n")
print(round(100 * tapply(def$desvio, def$entidade, mean), 1))

cat("\nProcessos seletivos: nenhum =", sum(d$processos == 0),
    sprintf("(%.1f%%)\n", 100 * mean(d$processos == 0)))
print(round(resumo_quant(d$processos), 2))
cat("Moda:", moda(d$processos), "\n")
print(round(sapply(split(d$processos, d$intencao3), function(v)
  c(media = mean(v), `% com 1 ou mais` = 100 * mean(v > 0))), 1))

titulo("Q6 – Faixa de média ponderada e expectativa salarial")
print(round(100 * prop.table(table(d$media3)), 1))
print(tab_pct(table(d$media3, d$intencao3)))

cat("\nExpectativa salarial – geral\n")
print(round(resumo_quant(d$salario), 2))
cat("Modas:", moda(d$salario), "\n")
cat("\nTabela 6 – Expectativa salarial por grupo de intenção atual\n")
t6 <- rbind(t(sapply(split(d$salario, d$intencao5), resumo_quant)), Total = resumo_quant(d$salario))
print(round(t6[, c("n", "media", "mediana", "dp", "cv", "min", "max", "Q1", "Q3", "aiq")], 3))
cat("\nVariâncias por grupo:\n"); print(round(tapply(d$salario, d$intencao5, var)))
