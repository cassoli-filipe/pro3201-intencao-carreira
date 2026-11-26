# Roda toda a análise e grava a saída em saidas/resultados.txt.
# Uso, a partir da pasta do repositório:  Rscript run_all.R

dir.create("saidas", showWarnings = FALSE)
saida <- file("saidas/resultados.txt", open = "wt", encoding = "UTF-8")
sink(saida, split = TRUE)
cat("Reprodução dos cálculos do relatório – PRO3201, Grupo 12-L (2026)\n")
cat(R.version.string, "\n")

for (f in c("R/00_funcoes.R", "R/01_preparacao.R", "R/02_descritiva.R",
            "R/03_inferencial.R", "R/04_robustez.R", "testes/conferencia.R")) {
  source(f, encoding = "UTF-8")
}

sink(); close(saida)
