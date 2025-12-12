# Manuscript

## render
quarto render index.qmd --to frpQuarto-html

## publish github
quarto publish gh-pages


https://peterfromberger.github.io/mtLancet/



In order to run the R scripts, there must be a _data folder at first level in folder structure (where the README.qmd file lives).

You will need some packages from Dr. Andreas Leha (Human medical center Göttingen):

- remotes::install_git("https://gitlab.gwdg.de/aleha/descsuppRplots", dependencies = FALSE)