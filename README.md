# Randomization List Generator

A Shiny web application for generating and evaluating randomization sequences in clinical trials, built on the [randomizeR](https://cran.r-project.org/package=randomizeR) R package.

Developed at **RealiseD** by Stefanie Schoenen and [Daniel Bodden](https://www.linkedin.com/in/daniel-bodden).

**Live app:** [realisedrandomizer.shinyapps.io/realisedrandomizer](https://realisedrandomizer.shinyapps.io/realisedrandomizer)

---

## Features

### Randomization Sequence Generation
Generate reproducible, documented randomization lists for clinical trials across seven procedures:

| Procedure | Category |
|---|---|
| Complete Randomization | Unbounded Randomization |
| Efron's Biased Coin Design | Unbounded Randomization |
| Permuted Block Randomization | Terminal Balance |
| Random Permuted Block Randomization | Terminal Balance |
| Random Allocation Rule | Terminal Balance |
| Big Stick Design | Maximum Tolerated Imbalance (MTI) |
| Chen's Design | Maximum Tolerated Imbalance (MTI) |

Each module supports:
- Reproducible sequences via user-defined seed
- Downloadable randomization list (CSV)
- Downloadable PDF report

### Bias Evaluation
Assess the statistical properties of randomization sequences under three criteria:

- **Imbalance** — distribution of absolute imbalance, signed imbalance, or loss across sequences
- **Selection Bias** — type I error inflation under the Convergent (CS) or Directional (DS) guessing strategy
- **Chronological Bias** — type I error inflation under linear, stepwise, or logarithmic time trends

Both exact assessment (full sequence space) and simulation-based assessment are supported. Results include a distribution plot, summary statistics, and the exact R code used to generate the assessment.

---

## Installation

### Requirements
- R >= 4.4.0
- The following R packages:

```r
install.packages(c("shiny", "bslib", "randomizeR", "ggplot2", "scales", "rmarkdown"))
```

### Run locally

```r
shiny::runApp("path/to/app")
```

---

## Project Structure

```
.
├── app.R           # Entry point
├── global.R        # Shared helpers and package loading
├── ui.R            # App layout and navigation
├── server.R        # Module server calls
├── modules/        # One file per randomization procedure / bias module
│   ├── mod_cr.R
│   ├── mod_ebc.R
│   ├── mod_pbr.R
│   ├── mod_rpbr.R
│   ├── mod_rar.R
│   ├── mod_bsd.R
│   ├── mod_chen.R
│   ├── mod_selbias.R
│   ├── mod_chronbias.R
│   └── mod_imbal.R
├── www/            # Static assets (CSS, logos)
│   └── style.css
└── Report/         # R Markdown report template
    └── Report.Rmd
```

---

## References

- Uschner D, Schindler D, Hilgers RD, Heussen N (2018). randomizeR: An R package for the assessment and implementation of randomization in clinical trials. *Journal of Statistical Software*, 85(8), 1–33. doi:[10.18637/jss.v085.i08](https://doi.org/10.18637/jss.v085.i08)
- Tamm M, Hilgers RD (2014). Chronological bias in randomized clinical trials arising from different types of unobserved time trends. *Methods of Information in Medicine*, 53(6), 501–510.
- Blackwell D, Hodges JL (1957). Design for the control of selection bias. *Annals of Mathematical Statistics*, 28(2), 449–460.
- Proschan M (1994). Influence of selection bias on type I error rate under random permuted block designs. *Statistica Sinica*, 4(1), 219–231.
- Berger VW (2005). *Selection Bias and Covariate Imbalances in Randomized Clinical Trials*. Wiley.

---

## License

See [LICENSE](LICENSE).
