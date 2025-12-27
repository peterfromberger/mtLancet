# Manuscript

## Render
To render the manuscript, use the following command:
```bash
quarto render index.qmd --to frpQuarto-html
```
## Publish to GitHub Pages
To publish the rendered manuscript to GitHub Pages, use the following command:
```bash
quarto publish gh-pages
```
The published manuscript can be accessed at: [https://peterfromberger.github.io/mtLancet/](https://peterfromberger.github.io/mtLancet/)

## Data Folder
In order to run some of the R scripts, ensure there is a `_data` folder at the first level in the folder structure (where the `README.md` file lives).

## Required R Packages
You will need some packages from Dr. Andreas Leha (Human Medical Center Göttingen):
```r
remotes::install_git("https://gitlab.gwdg.de/aleha/descsuppRplots", dependencies = FALSE)
```