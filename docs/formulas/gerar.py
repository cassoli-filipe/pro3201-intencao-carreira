# Gera as fórmulas da seção 2 (Fundamentação teórica) do relatório como PNG.
# Uso: uv run --with matplotlib python docs/formulas/gerar.py   (na raiz do repositório)
import json, pathlib
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

matplotlib.rcParams["mathtext.fontset"] = "stix"
TAM = 12      # pt: mesmo corpo do texto do relatório
DPI = 600
OUT = pathlib.Path(__file__).parent
Q = r"\qquad"

FORMULAS = {
    "01_ic_proporcao":   r"IC(p;\,1-\alpha) = \hat{p} \pm z_{\alpha/2}\sqrt{\dfrac{\hat{p}(1-\hat{p})}{n}}",
    "02_teste_proporcao": r"Z = \dfrac{\hat{p}-p_0}{\sqrt{\dfrac{p_0(1-p_0)}{n}}}",
    "03a_duas_proporcoes_teste": r"Z = \dfrac{\hat{p}_1-\hat{p}_2}{\sqrt{\hat{p}(1-\hat{p})\left(\dfrac{1}{n_1}+\dfrac{1}{n_2}\right)}}," + Q
                                 + r"\hat{p} = \dfrac{n_1\hat{p}_1+n_2\hat{p}_2}{n_1+n_2}",
    "03b_duas_proporcoes_ic": r"\hat{p}_1-\hat{p}_2 \pm z_{\alpha/2}\sqrt{\dfrac{\hat{p}_1(1-\hat{p}_1)}{n_1}+\dfrac{\hat{p}_2(1-\hat{p}_2)}{n_2}}",
    "04_qui_quadrado":   r"Q = \sum_{i=1}^{I}\;\sum_{j=1}^{J}\dfrac{(O_{ij}-E_{ij})^2}{E_{ij}}," + Q + r"E_{ij} = \dfrac{n_{i\bullet}\,n_{\bullet j}}{n}",
    "05_kolmogorov":     r"K = \max\left|F_0(x)-F_n(x)\right|",
    "06_anova":          r"F = \dfrac{\mathrm{SQE}/(k-1)}{\mathrm{SQD}/(n-k)}," + Q
                         + r"\mathrm{SQE} = \sum_{i=1}^{k} n_i(\overline{X}_i-\overline{X})^2," + Q
                         + r"\mathrm{SQD} = \sum_{i=1}^{k}(n_i-1)\,S_i^2",
    "07_bartlett_scheffe": r"D = \sum_{i=1}^{k}(n_i-1)\,\ln\dfrac{S_p^2}{S_i^2}," + Q
                         + r"\delta_{ij} = \sqrt{(k-1)\,F_c\,S_p^2\left(\dfrac{1}{n_i}+\dfrac{1}{n_j}\right)}",
}

tamanhos = {}
for nome, tex in FORMULAS.items():
    fig = plt.figure()
    fig.text(0, 0, f"${tex}$", fontsize=TAM)
    arq = OUT / f"{nome}.png"
    fig.savefig(arq, dpi=DPI, bbox_inches="tight", pad_inches=0.03, transparent=False, facecolor="white")
    plt.close(fig)
    from PIL import Image
    w, h = Image.open(arq).size
    tamanhos[nome] = {"largura_pt": round(w / DPI * 72, 1), "altura_pt": round(h / DPI * 72, 1)}
    print(f"{nome}: {tamanhos[nome]}")
(OUT / "tamanhos.json").write_text(json.dumps(tamanhos, indent=1))
