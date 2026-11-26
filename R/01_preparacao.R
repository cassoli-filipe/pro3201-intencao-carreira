# Leitura da planilha codificada, exclusão das respostas fora da população
# (etapa 5 do Apêndice C) e criação das variáveis derivadas e dos agrupamentos
# descritos na seção 3.2 do relatório.

ARQUIVO_DADOS <- Sys.getenv("ARQUIVO_DADOS", "dados/respostas_codificadas.csv")

# Nome de cada variável na planilha (à direita), se for diferente do nome canônico.
COLUNAS <- c(
  id                = "id",
  data_envio        = "data_envio",
  ingresso          = "ingresso",
  intercambio       = "intercambio",
  curso             = "curso",
  media_faixa       = "media_faixa",
  entidade          = "entidade",
  processos         = "processos",
  intencao_atual    = "intencao_atual",
  intencao_ingresso = "intencao_ingresso",
  certeza           = "certeza",
  salario           = "salario"
)

INTENCOES <- c("Engenharia", "Tecnologia", "Mercado financeiro", "Consultoria",
               "Empreendedorismo", "Outro", "Não sabe")

rotulo_intencao <- function(x) {
  x <- trimws(x); out <- rep(NA_character_, length(x))
  out[grepl("^Engenharia", x)]                  <- "Engenharia"
  out[grepl("^Tecnologia", x)]                  <- "Tecnologia"
  out[grepl("financeiro", x, ignore.case = TRUE)] <- "Mercado financeiro"
  out[grepl("^Consultoria", x)]                 <- "Consultoria"
  out[grepl("^Empreendedorismo", x)]            <- "Empreendedorismo"
  out[grepl("^Outro", x)]                       <- "Outro"
  out[grepl("^(Ainda n|Não sab|Nao sab)", x)]   <- "Não sabe"
  if (anyNA(out)) stop("Intenção não reconhecida: ", paste(unique(x[is.na(out)]), collapse = " | "))
  factor(out, levels = INTENCOES)
}

rotulo_curso <- function(x) {
  padroes <- c(Ambiental = "Ambiental", Civil = "Civil", "Computação" = "Computa",
               "Elétrica" = "El[ée]trica", Materiais = "Materiais", "Mecânica" = "Mec[âa]nica",
               "Mecatrônica" = "Mecatr", "Metalúrgica" = "Metal", Minas = "Minas",
               Naval = "Naval", "Produção" = "Produ", "Química" = "Qu[íi]mica", Outro = "^Outro")
  out <- rep(NA_character_, length(x))
  for (nm in names(padroes)) out[is.na(out) & grepl(padroes[[nm]], x)] <- nm
  if (anyNA(out)) stop("Curso não reconhecido: ", paste(unique(x[is.na(out)]), collapse = " | "))
  out
}

# Faixa de média ponderada: o primeiro número da faixa ("5,0–5,9" -> 5); "Abaixo de 5,0" -> 4
faixa_media <- function(x) {
  x <- trimws(x)
  lim <- ifelse(grepl("^(Abaixo|<)", x), 4,
                as.numeric(sub(",", ".", regmatches(x, regexpr("[0-9]+(,[0-9])?", x)))))
  if (anyNA(lim)) stop("Faixa de média não reconhecida")
  factor(c(`4` = "< 5,0", `5` = "5,0–5,9", `6` = "6,0–6,9", `7` = "7,0–7,9",
           `8` = "8,0–8,9", `9` = "9,0–10,0")[as.character(floor(lim))],
         levels = c("< 5,0", "5,0–5,9", "6,0–6,9", "7,0–7,9", "8,0–8,9", "9,0–10,0"))
}

numero <- function(x) {             # "R$ 10.000" ou "10000" -> 10000
  if (is.numeric(x)) return(x)
  as.numeric(sub(",", ".", gsub("[^0-9,]", "", x)))
}

ler_dados <- function(arquivo = ARQUIVO_DADOS) {
  bruto <- read.csv(arquivo, fileEncoding = "UTF-8", check.names = FALSE,
                    stringsAsFactors = FALSE)
  faltam <- setdiff(COLUNAS, names(bruto))
  faltam <- faltam[!(faltam %in% COLUNAS[c("intercambio", "data_envio")])]  # opcionais
  if (length(faltam)) stop("Colunas ausentes na planilha: ", paste(faltam, collapse = ", "))
  col <- function(v) if (COLUNAS[[v]] %in% names(bruto)) bruto[[COLUNAS[[v]]]] else NA
  data.frame(
    id                = col("id"),
    data_envio        = as.Date(col("data_envio")),
    ingresso          = trimws(as.character(col("ingresso"))),
    intercambio       = trimws(as.character(col("intercambio"))),
    curso             = rotulo_curso(col("curso")),
    media_faixa       = faixa_media(col("media_faixa")),
    entidade          = factor(trimws(col("entidade")), levels = c("Sim", "Não")),
    processos         = numero(col("processos")),
    intencao_atual    = rotulo_intencao(col("intencao_atual")),
    intencao_ingresso = rotulo_intencao(col("intencao_ingresso")),
    certeza           = numero(col("certeza")),
    salario           = numero(col("salario")),
    stringsAsFactors  = FALSE
  )
}

preparar <- function(todos) {
  na_pop <- todos$ingresso %in% c("2024", "2025", "2026") &
    !(todos$intercambio %in% c("Sim", "sim"))
  d <- todos[na_pop, ]
  d$ingresso <- factor(d$ingresso)

  fora <- c("Mercado financeiro", "Consultoria", "Empreendedorismo", "Outro")
  desvio <- function(x) ifelse(x == "Não sabe", NA, as.integer(x %in% fora))
  d$desvio        <- desvio(d$intencao_atual)       # 1 = fora de engenharia/tecnologia
  d$desvio_ing    <- desvio(d$intencao_ingresso)
  d$mudou         <- as.integer(as.character(d$intencao_atual) != as.character(d$intencao_ingresso))
  d$ln_salario    <- log(d$salario)

  # Agrupamentos para garantir Eij > 5 (seção 3.2)
  d$intencao3 <- factor(ifelse(d$intencao_atual %in% c("Engenharia", "Tecnologia"), "Engenharia/Tecnologia",
                        ifelse(d$intencao_atual %in% c("Mercado financeiro", "Consultoria"),
                               "Mercado financeiro/Consultoria", "Outros/Indefinido")),
                        levels = c("Engenharia/Tecnologia", "Mercado financeiro/Consultoria", "Outros/Indefinido"))
  d$intencao5 <- factor(ifelse(d$intencao_atual %in% c("Empreendedorismo", "Outro", "Não sabe"),
                               "Outros/Indefinido", as.character(d$intencao_atual)),
                        levels = c("Engenharia", "Tecnologia", "Mercado financeiro", "Consultoria", "Outros/Indefinido"))
  grupo_curso <- c("Produção" = "Produção", "Elétrica" = "Elétrica/Computação", "Computação" = "Elétrica/Computação",
                   Civil = "Civil/Ambiental", Ambiental = "Civil/Ambiental",
                   "Mecânica" = "Mecânica/Mecatrônica/Naval", "Mecatrônica" = "Mecânica/Mecatrônica/Naval",
                   Naval = "Mecânica/Mecatrônica/Naval",
                   "Química" = "Química/Materiais/Metalúrgica/Minas", Materiais = "Química/Materiais/Metalúrgica/Minas",
                   "Metalúrgica" = "Química/Materiais/Metalúrgica/Minas", Minas = "Química/Materiais/Metalúrgica/Minas",
                   Outro = NA)
  d$curso5 <- factor(unname(grupo_curso[d$curso]), levels = unique(na.omit(grupo_curso)))
  d$media3 <- factor(ifelse(d$media_faixa %in% c("< 5,0", "5,0–5,9", "6,0–6,9"), "Abaixo de 7,0",
                     ifelse(d$media_faixa == "7,0–7,9", "7,0 a 7,9", "8,0 ou mais")),
                     levels = c("Abaixo de 7,0", "7,0 a 7,9", "8,0 ou mais"))
  d$processos3 <- cut(d$processos, c(-Inf, 0, 3, Inf), labels = c("Nenhum", "1 a 3", "4 ou mais"))
  d$certeza3   <- cut(d$certeza, c(0, 2, 3, 5), labels = c("Baixa (1-2)", "Média (3)", "Alta (4-5)"))
  d
}

todos <- ler_dados()
d <- preparar(todos)

titulo("Preparação dos dados (Apêndice C, etapa 5)")
cat("Respostas na planilha codificada:", nrow(todos), "\n")
if (!all(is.na(todos$data_envio))) {
  ult3 <- todos$data_envio >= max(todos$data_envio) - 2
  cat(sprintf("Enviadas nos três últimos dias de coleta: %d (%.1f%%)\n", sum(ult3), 100 * mean(ult3)))
}
cat("Fora da população (outro ano de ingresso ou intercâmbio):", nrow(todos) - nrow(d), "\n")
cat("Observações válidas:", nrow(d), "\n")
cat("Campos em branco nas válidas:",
    sum(is.na(d[, setdiff(names(COLUNAS), c("intercambio", "data_envio"))])), "\n")
