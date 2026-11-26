# Intenção de carreira dos estudantes da Poli-USP: reprodução dos cálculos em R

Repositório de apoio ao relatório final do Projeto de Análise Estatística da disciplina PRO3201 – Estatística Básica e Aplicações (Escola Politécnica da USP, 2026), Grupo 12-L. O título do relatório é *"Engenharia, mercado financeiro ou consultoria? A formação da intenção de carreira dos estudantes da Escola Politécnica da USP"*.

O relatório foi calculado no Excel. Este repositório refaz todos os cálculos em R, de forma independente, a partir das respostas codificadas. Ao final, confere cada número publicado no relatório: **159 de 159 conferem**.

## Como rodar

Requer R ≥ 4.0, sem pacotes adicionais (só R base). Na pasta do repositório:

```bash
Rscript run_all.R
```

A saída completa vai para [`saidas/resultados.txt`](saidas/resultados.txt), que também está versionada, para leitura sem rodar.

As figuras 1 a 5 do relatório saem de outro comando, também só com R base:

```bash
Rscript R/05_figuras.R
```

Cada figura é gravada em [`figuras/`](figuras/) em dois formatos: PNG a 300 dpi, para colar em documentos, e PDF vetorial, com a fonte Arial embutida. O estilo é sóbrio: tons de cinza, sem título dentro do gráfico (a legenda fica no relatório) e rótulos com vírgula decimal.

| Arquivo | Figura do relatório |
|---|---|
| `figura_1` | Intenção de carreira atual dos estudantes |
| `figura_2` | Intenção de carreira no ingresso e atualmente |
| `figura_3` | Grau de certeza sobre a carreira por turma de ingresso |
| `figura_4` | Intenção de carreira atual (três grupos) por grupo de curso |
| `figura_5` | Expectativa salarial bruta por grupo de intenção atual (boxplot) |

No boxplot, os bigodes vão até o último valor dentro de 1,5 × AIQ, e os círculos marcam os valores acima disso, a mesma regra de "possível valor atípico" dos slides da disciplina. As fórmulas da seção 2 do relatório estão em [`docs/formulas/`](docs/formulas/), geradas por `docs/formulas/gerar.py`.

## Estrutura

| Arquivo | O que faz | Seção do relatório |
|---|---|---|
| `dados/respostas_codificadas.csv` | As 210 respostas após a limpeza (etapas 1 a 4 do Apêndice C) | Apêndices A e C |
| `R/00_funcoes.R` | Testes e estimadores na forma dos slides e do formulário de PRO3200 | Seção 2 |
| `R/01_preparacao.R` | Exclui as 6 respostas fora da população (210 → 204), recodifica e cria os agrupamentos | Seção 3.2 e Apêndice C |
| `R/02_descritiva.R` | Tabelas de frequência, matriz de transição e medidas-resumo | Seções 4.1 e 4.2 |
| `R/03_inferencial.R` | IC e testes para proporções, qui-quadrado, Bartlett, KS, ANOVA e Scheffé | Seção 4.3 |
| `R/04_robustez.R` | Verificações extras que não estão no relatório (ver abaixo) | — |
| `R/05_figuras.R` | Figuras 1 a 5, em PNG e PDF | Seção 4.2 |
| `testes/conferencia.R` | Compara cada número recalculado com o publicado no relatório | — |

## Dados

Cada linha é a resposta de um estudante ao questionário (Apêndice B do relatório), com os textos das opções exatamente como saíram do Google Forms.

| Coluna | Conteúdo |
|---|---|
| `id` | Código sequencial do respondente (R001 a R210) |
| `data_envio` | Data do envio (sem horário) |
| `ingresso` | 2024, 2025, 2026 ou "Outro ano" |
| `intercambio` | Sim/Não |
| `curso` | Curso de engenharia |
| `media_faixa` | Faixa de média ponderada no Júpiter |
| `entidade` | Participa ou participou de entidade estudantil (Sim/Não) |
| `processos` | Número de processos seletivos de estágio ou emprego |
| `intencao_atual`, `intencao_ingresso` | Área em que pretende trabalhar hoje e a que pretendia no ingresso |
| `certeza` | Grau de certeza sobre a carreira, de 1 (nada certo) a 5 (totalmente certo) |
| `salario` | Expectativa de salário mensal bruto no primeiro emprego (R$) |

**Privacidade.** O número USP foi usado só para eliminar envios duplicados e foi descartado antes da codificação. O arquivo não tem número USP, nome, e-mail nem horário de envio.

## Convenções seguidas

Os cálculos seguem as fórmulas dos slides e do formulário de prova da disciplina:

- **IC para proporção:** usa p̂ no erro padrão.
- **Teste para uma proporção:** usa p₀ no erro padrão.
- **Teste para duas proporções:** usa a proporção combinada; o IC da diferença usa as proporções separadas.
- **Qui-quadrado:** sem correção de Yates, exigindo frequência esperada Eij > 5.
- **Bartlett:** D = Σ(nᵢ−1)·ln(Sp²/Sᵢ²), sem o fator de correção.
- **Kolmogorov-Smirnov:** valor crítico aproximado 1,36/√n.
- **Scheffé:** δᵢⱼ = √[(k−1)·F𝒸·Sp²·(1/nᵢ + 1/nⱼ)].
- **Quartis:** pela convenção do slide de estatística descritiva; neste conjunto de dados coincidem com os do Excel em todos os valores publicados.

Sempre que existe função equivalente no R (`chisq.test`, `aov`, `bartlett.test`), o resultado manual é conferido contra ela.

**Desvio ocupacional** é a variável derivada que vale 1 quando o estudante pretende atuar fora de engenharia e tecnologia (mercado financeiro, consultoria, empreendedorismo ou outro) e 0 quando pretende atuar em engenharia ou tecnologia. "Ainda não sei" fica como ausente.

## Verificações extras (`R/04_robustez.R`)

| | Verificação | Resultado |
|---|---|---|
| R1 | Mudança de intenção só entre quem tinha intenção definida nos dois momentos | 57 de 162 (35,2%) |
| R2 | Proporção de desvio ponderada pelos estratos (partilha igual), supondo turmas de tamanho semelhante | 48,4%, contra 48,9% sem ponderar |
| R3 | Q3 pelo teste de McNemar | Q = 18,69 = Z² do teste para uma proporção |
| R4 | Q6 com Bartlett corrigido, ANOVA na escala original, Kruskal-Wallis e Shapiro-Wilk | As conclusões se mantêm |
| R5 | Proporção de desvio se "Outro" (pesquisa, setor público) contasse como dentro da área | 42,9% |
| R6 | Regressão linear simples de ln(salário) em função do número de processos seletivos | Exploratória |
