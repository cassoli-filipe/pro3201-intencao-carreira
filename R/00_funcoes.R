# Funções estatísticas escritas na forma dos slides de PRO3200 (Poli-USP, 2026)
# e do formulário de prova. Só R base. Cada função devolve os números que o
# relatório apresenta: estatística, valor crítico, valor-p e decisão sobre H0.

alfa <- 0.05

# ---------------------------------------------------------------- proporções

# IC para uma proporção: p̂ ± z(α/2)·√(p̂(1−p̂)/n)  (P1-4, slide 38; formulário)
ic_prop <- function(x, n, conf = 1 - alfa) {
  p <- x / n
  e <- qnorm(1 - (1 - conf) / 2) * sqrt(p * (1 - p) / n)
  c(p = p, inf = p - e, sup = p + e)
}

# Teste para uma proporção, com p0 no erro padrão (P2-6, slides 39-43; formulário)
teste_prop <- function(x, n, p0, alternativa = c("bilateral", "menor", "maior")) {
  alternativa <- match.arg(alternativa)
  p <- x / n
  z <- (p - p0) / sqrt(p0 * (1 - p0) / n)
  crit <- switch(alternativa,
                 bilateral = qnorm(1 - alfa / 2),
                 menor     = qnorm(alfa),
                 maior     = qnorm(1 - alfa))
  valor_p <- switch(alternativa,
                    bilateral = 2 * pnorm(-abs(z)),
                    menor     = pnorm(z),
                    maior     = pnorm(z, lower.tail = FALSE))
  rejeita <- switch(alternativa,
                    bilateral = abs(z) > crit,
                    menor     = z < crit,
                    maior     = z > crit)
  list(x = x, n = n, p = p, z = z, critico = crit, valor_p = valor_p,
       rejeita_H0 = rejeita)
}

# Duas proporções independentes (P2-5, slides 61-63; P2-7, slides 24-26; formulário):
# o teste usa p̂ combinado; o IC da diferença usa p̂1 e p̂2 separados.
teste_duas_prop <- function(x1, n1, x2, n2, conf = 1 - alfa) {
  p1 <- x1 / n1; p2 <- x2 / n2
  pc <- (x1 + x2) / (n1 + n2)
  z <- (p1 - p2) / sqrt(pc * (1 - pc) * (1 / n1 + 1 / n2))
  crit <- qnorm(1 - alfa / 2)
  e <- qnorm(1 - (1 - conf) / 2) * sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  list(p1 = p1, p2 = p2, p_combinado = pc, z = z, critico = crit,
       valor_p = 2 * pnorm(-abs(z)), rejeita_H0 = abs(z) > crit,
       ic_dif = c(inf = p1 - p2 - e, sup = p1 - p2 + e))
}

# ------------------------------------------------------------- qui-quadrado

# Teste de associação: Q = ΣΣ (Oij − Eij)²/Eij, Eij = ni.·n.j/n,
# gl = (I−1)(J−1); a aproximação exige Eij > 5 (P2-8, slides 16-18; formulário).
qui_quadrado <- function(tab) {
  tab <- as.matrix(tab)
  E <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  Q <- sum((tab - E)^2 / E)
  gl <- (nrow(tab) - 1) * (ncol(tab) - 1)
  crit <- qchisq(1 - alfa, gl)
  # conferência com a função do R, sem correção de Yates (que os slides não usam)
  ref <- suppressWarnings(chisq.test(tab, correct = FALSE))
  stopifnot(isTRUE(all.equal(Q, unname(ref$statistic))))
  list(n = sum(tab), Q = Q, gl = gl, critico = crit,
       valor_p = pchisq(Q, gl, lower.tail = FALSE), menor_E = min(E),
       rejeita_H0 = Q > crit)
}

# --------------------------------------------------------------------- ANOVA

# ANOVA com um fator: SQE (entre), SQD (dentro), SQT; F = Se²/Sp² (P3-9; formulário)
anova_1f <- function(y, g) {
  g <- droplevels(as.factor(g))
  ni <- tapply(y, g, length); mi <- tapply(y, g, mean)
  k <- nlevels(g); N <- length(y)
  SQE <- sum(ni * (mi - mean(y))^2)
  SQD <- sum(tapply(y, g, function(v) sum((v - mean(v))^2)))
  SQT <- sum((y - mean(y))^2)
  Se2 <- SQE / (k - 1); Sp2 <- SQD / (N - k)
  Fobs <- Se2 / Sp2; Fc <- qf(1 - alfa, k - 1, N - k)
  # conferência com aov()
  ref <- summary(aov(y ~ g))[[1]]
  stopifnot(isTRUE(all.equal(Fobs, ref[["F value"]][1])))
  list(k = k, N = N, ni = ni, medias = mi, SQE = SQE, SQD = SQD, SQT = SQT,
       gl = c(entre = k - 1, dentro = N - k, total = N - 1),
       Se2 = Se2, Sp2 = Sp2, F = Fobs, Fc = Fc,
       valor_p = pf(Fobs, k - 1, N - k, lower.tail = FALSE),
       rejeita_H0 = Fobs > Fc,
       residuos = y - ave(y, g))
}

# Teste de Bartlett na forma do slide: D = Σ (ni − 1)·ln(Sp²/Si²) ~ χ²(k−1)
# (P3-9, slide 20). O slide não usa o fator de correção C; o valor com C
# (o de bartlett.test()) é devolvido só para comparação.
bartlett_slide <- function(y, g) {
  g <- droplevels(as.factor(g))
  ni <- tapply(y, g, length); Si2 <- tapply(y, g, var)
  k <- nlevels(g); N <- length(y)
  Sp2 <- sum((ni - 1) * Si2) / (N - k)
  D <- sum((ni - 1) * log(Sp2 / Si2))
  C <- 1 + (sum(1 / (ni - 1)) - 1 / (N - k)) / (3 * (k - 1))
  stopifnot(isTRUE(all.equal(D / C, unname(bartlett.test(y, g)$statistic))))
  crit <- qchisq(1 - alfa, k - 1)
  list(D = D, gl = k - 1, critico = crit,
       valor_p = pchisq(D, k - 1, lower.tail = FALSE), rejeita_H0 = D > crit,
       D_com_correcao = D / C)
}

# Kolmogorov-Smirnov para normalidade dos resíduos: K = max |F0(x) − Fn(x)|
# (P2-8, slide 5), com F0 = N(0, s) e s = desvio padrão amostral dos resíduos.
# Valor crítico aproximado 1,36/√n (α = 5%).
ks_residuos <- function(res) {
  n <- length(res); r <- sort(res)
  F0 <- pnorm(r, 0, sd(res)); i <- seq_len(n)
  K <- max(i / n - F0, F0 - (i - 1) / n)
  crit <- 1.36 / sqrt(n)
  list(K = K, n = n, critico = crit, rejeita_H0 = K > crit)
}

# Método de Scheffé: médias i e j diferem se |X̄i − X̄j| > δij,
# δij = √[(k−1)·Fc·Sp²·(1/ni + 1/nj)]  (P3-9, slide 18)
scheffe <- function(av) {
  nomes <- names(av$medias); pares <- combn(nomes, 2)
  out <- data.frame(
    par = paste(pares[1, ], "x", pares[2, ]),
    dif_abs = abs(av$medias[pares[1, ]] - av$medias[pares[2, ]]),
    delta = sqrt((av$k - 1) * av$Fc * av$Sp2 *
                   (1 / av$ni[pares[1, ]] + 1 / av$ni[pares[2, ]])),
    row.names = NULL)
  out$diferem <- ifelse(out$dif_abs > out$delta, "Sim", "Não")
  out
}

# ---------------------------------------------------------------- descritiva

# Quartil pela convenção do slide (P1-2, slide 37): i = p·n; se i não é inteiro,
# usa a posição seguinte; se é inteiro, a média das posições i e i+1.
quartil_slide <- function(x, p) {
  x <- sort(x); i <- p * length(x)
  if (abs(i - round(i)) > 1e-9) x[ceiling(i)] else (x[i] + x[i + 1]) / 2
}

moda <- function(x) {
  t <- table(x); as.numeric(names(t)[t == max(t)])
}

# Coeficiente de assimetria igual ao da função DISTORÇÃO do Excel
assimetria_excel <- function(x) {
  n <- length(x)
  n / ((n - 1) * (n - 2)) * sum(((x - mean(x)) / sd(x))^3)
}

resumo_quant <- function(x) {
  c(n = length(x), media = mean(x),
    Q1 = quartil_slide(x, .25), mediana = median(x), Q3 = quartil_slide(x, .75),
    dp = sd(x), cv = sd(x) / mean(x), aiq = quartil_slide(x, .75) - quartil_slide(x, .25),
    min = min(x), max = max(x), assimetria = assimetria_excel(x))
}

# ------------------------------------------------------------------ impressão

titulo <- function(txt) cat("\n", strrep("=", 72), "\n", txt, "\n", strrep("=", 72), "\n", sep = "")
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = ",")
mostra <- function(nome, r) {
  campos <- r[!vapply(r, function(v) length(v) > 3, logical(1))]
  cat(sprintf("%-38s %s\n", nome,
              paste(names(campos), vapply(campos, function(v)
                if (is.logical(v)) as.character(v) else paste(fmt(v, 4), collapse = "; "),
                character(1)), sep = " = ", collapse = " | ")))
}
