# Figuras 1 a 5 do relatório, só com R base (pacotes graphics e grDevices).
# Uso, a partir da pasta do repositório:  Rscript R/05_figuras.R
# Saída: figuras/figura_N.png (300 dpi) e figuras/figura_N.pdf (vetorial).

source("R/00_funcoes.R", encoding = "UTF-8")
invisible(capture.output(source("R/01_preparacao.R", encoding = "UTF-8")))
dir.create("figuras", showWarnings = FALSE)

# ------------------------------------------------------------------ estilo
FONTE  <- "Arial"
LARG   <- 453.5 / 72 * 2.54   # cm: largura das figuras no relatório (453,5 pt)
PT     <- function(pt) pt / 72 * 2.54   # alturas do relatório, em pt -> cm
GRADE  <- "#E0E0E0"
TEXTO  <- "#000000"
CINZA1 <- "#595959"                                     # barras simples
CINZA2 <- c("#BFBFBF", "#595959")                       # duas categorias
CINZA3 <- c("#3D3D3D", "#9E9E9E", "#E7E7E7")            # três categorias
CINZA5 <- c("#E6E6E6", "#C4C4C4", "#9E9E9E", "#6E6E6E", "#3D3D3D")

pct <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d, decimal.mark = ","), "%")
contraste <- function(cor) {                            # texto branco sobre cinza escuro
  rgb <- col2rgb(cor); ifelse(colSums(rgb * c(.299, .587, .114)) < 140, "white", TEXTO)
}

salvar <- function(nome, altura, desenho) {
  for (tipo in c("png", "pdf")) {
    arq <- file.path("figuras", paste0(nome, ".", tipo))
    if (tipo == "png") png(arq, width = LARG, height = altura, units = "cm", res = 300,
                           type = "cairo", family = FONTE, pointsize = 8.35)
    else cairo_pdf(arq, width = LARG / 2.54, height = altura / 2.54, family = FONTE, pointsize = 8.35)
    par(col.axis = TEXTO, col.lab = TEXTO, fg = TEXTO, las = 1, mgp = c(1.9, 0.5, 0), tcl = -0.25, xaxs = "i")
    desenho()
    dev.off()
  }
  cat("figuras/", nome, ".png e .pdf\n", sep = "")
}


# margem esquerda (em linhas) suficiente para o maior rótulo do eixo vertical
margem <- function(rotulos) max(strwidth(rotulos, units = "inches", cex = 1)) / par("csi") + 0.5

# legenda horizontal no topo da figura, alinhada à margem esquerda
legenda_topo <- function(rotulos, ...) {
  op <- par(xpd = NA); on.exit(par(op))
  legend(x = grconvertX(0.002, "ndc", "user"), y = grconvertY(0.995, "ndc", "user"), legend = rotulos,
         horiz = TRUE, bty = "n", cex = 1, x.intersp = 0.5,
         text.width = strwidth(rotulos) + strwidth("   "), ...)
}

rotulos_int <- c("Engenharia" = "Engenharia", "Tecnologia" = "Tecnologia",
                 "Mercado financeiro" = "Mercado financeiro", "Consultoria" = "Consultoria",
                 "Empreendedorismo" = "Empreendedorismo", "Outro" = "Outro", "Não sabe" = "Ainda não sei")

# ------------------------------------------------------------- Figura 1
salvar("figura_1", PT(163), function() {
  f <- sort(table(d$intencao_atual), decreasing = FALSE)          # menor embaixo
  par(mar = c(2.9, margem(rotulos_int), 0.4, 2.5))
  b <- barplot(as.numeric(f), horiz = TRUE, col = CINZA1, border = NA, xlim = c(0, 100),
               axes = FALSE, space = 0.45)
  abline(v = seq(20, 100, 20), col = GRADE, lwd = 0.8)
  barplot(as.numeric(f), horiz = TRUE, col = CINZA1, border = NA, axes = FALSE, space = 0.45, add = TRUE)
  axis(1, at = seq(0, 100, 20), lwd = 0, lwd.ticks = 0.6)
  axis(2, at = b, labels = rotulos_int[names(f)], lwd = 0, line = -0.2)
  abline(v = 0, lwd = 0.8)
  text(as.numeric(f), b, paste0(as.numeric(f), " (", pct(as.numeric(f) / nrow(d)), ")"), pos = 4, cex = 1, xpd = TRUE)
  title(xlab = sprintf("Número de estudantes (n = %d)", nrow(d)))
})

# ------------------------------------------------------------- Figura 2
salvar("figura_2", PT(233), function() {
  ordem <- names(sort(table(d$intencao_atual), decreasing = TRUE))
  m <- rbind(atual = table(d$intencao_atual)[ordem], ingresso = table(d$intencao_ingresso)[ordem]) / nrow(d)
  m <- m[, rev(ordem)]                                              # primeira categoria no topo
  par(mar = c(2.9, margem(rotulos_int), 1.8, 2.5))
  b <- barplot(100 * m, beside = TRUE, horiz = TRUE, col = c(CINZA2[2], CINZA2[1]), border = NA,
               xlim = c(0, 60), axes = FALSE, space = c(0.05, 0.55), names.arg = rep("", ncol(m)))
  abline(v = seq(10, 60, 10), col = GRADE, lwd = 0.8)
  barplot(100 * m, beside = TRUE, horiz = TRUE, col = c(CINZA2[2], CINZA2[1]), border = NA,
          axes = FALSE, space = c(0.05, 0.55), names.arg = rep("", ncol(m)), add = TRUE)
  axis(1, at = seq(0, 60, 10), lwd = 0, lwd.ticks = 0.6)
  axis(2, at = colMeans(b), labels = rotulos_int[colnames(m)], lwd = 0, line = -0.2)
  abline(v = 0, lwd = 0.8)
  text(100 * m, b, pct(m), pos = 4, cex = 1, xpd = TRUE)
  title(xlab = sprintf("Percentual dos estudantes (%%; n = %d)", nrow(d)))
  legenda_topo(c("no ingresso", "atualmente"), fill = CINZA2, border = NA)
})

# ---------------------------------------------- empilhadas em 100% (Figuras 3 e 4)
empilhadas <- function(tab, cores, legenda, xlab, rot_linhas) {
  p <- prop.table(tab, 1)
  p <- p[rev(seq_len(nrow(p))), , drop = FALSE]                    # primeira linha no topo
  par(mar = c(2.9, margem(rot_linhas), 1.9, 1.2))
  b <- barplot(t(100 * p), horiz = TRUE, col = cores, border = "white", lwd = 0.6, xlim = c(0, 100),
               axes = FALSE, space = 0.35, names.arg = rep("", nrow(p)))
  axis(1, at = seq(0, 100, 20), lwd = 0, lwd.ticks = 0.6)
  axis(2, at = b, labels = rev(rot_linhas), lwd = 0, line = -0.2)
  for (i in seq_len(nrow(p))) {
    x0 <- c(0, cumsum(100 * p[i, ]))[-(ncol(p) + 1)]; meio <- x0 + 100 * p[i, ] / 2
    mostra <- p[i, ] >= 0.05
    text(meio[mostra], b[i], pct(p[i, mostra], 0), col = contraste(cores)[mostra], cex = 1)
  }
  title(xlab = xlab)
  legenda_topo(legenda, fill = cores, border = "#9E9E9E")
}

salvar("figura_3", PT(123), function() {
  tab <- table(d$ingresso, factor(d$certeza, levels = 1:5))
  n <- rowSums(tab)
  empilhadas(tab, CINZA5, c("1 – nada certo", "2", "3", "4", "5 – totalmente certo"),
             "Percentual dos estudantes da turma de ingresso (%)",
             sprintf("%s (n = %d)", rownames(tab), n))
})

salvar("figura_4", PT(176), function() {
  ordem <- c("Produção", "Elétrica/Computação", "Civil/Ambiental", "Mecânica/Mecatrônica/Naval",
             "Química/Materiais/Metalúrgica/Minas")
  tab <- table(factor(d$curso5, levels = ordem), d$intencao3)
  empilhadas(tab, CINZA3, levels(d$intencao3), "Percentual dos estudantes do grupo de curso (%)",
             sprintf("%s (n = %d)", ordem, rowSums(tab)))
})

# ------------------------------------------------------------- Figura 5
salvar("figura_5", PT(202), function() {
  grupos <- levels(d$intencao5); y <- d$salario / 1000
  n <- table(d$intencao5)
  par(mar = c(3.6, 3.6, 1.6, 0.6))
  plot.new(); plot.window(xlim = c(0.5, length(grupos) + 0.5), ylim = c(0, 31))
  abline(h = seq(5, 30, 5), col = "#D9D9D9", lwd = 0.8)
  boxplot(y ~ d$intencao5, add = TRUE, axes = FALSE, col = "white", border = TEXTO, boxwex = 0.5,
          medlwd = 2, whisklty = 1, staplelwd = 0.8, outpch = 1, outcex = 0.8, lwd = 0.8)
  points(seq_along(grupos), tapply(y, d$intencao5, mean), pch = 18, cex = 1.2, col = TEXTO)
  axis(2, at = seq(0, 30, 5), lwd = 0, lwd.ticks = 0.6)
  axis(1, at = seq_along(grupos), labels = sprintf("%s\n(n = %d)", grupos, n[grupos]), lwd = 0, padj = 0.6)
  abline(h = 0, lwd = 0.8)
  title(ylab = "Expectativa salarial bruta (R$ mil/mês)")
  legenda_topo(c("mediana", "média"), lty = c(1, NA), lwd = c(2, NA), pch = c(NA, 18), pt.cex = 1.2)
})
