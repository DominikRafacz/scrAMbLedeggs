# scrAMbLedeggs

To perform all calculations in projects, you have to install packages `renv` and `targets`. Then run:

```r
renv::activate()
renv::restore()
targets::tar_make()
```

# Datasets used:

## breast

Dataset comes from
[UCI](https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(original)).

We are using wdbc.data file with features 2-12. The first one is target: 1 (malignant) or 0 (benign). All the others are numeric variables.

## credit-g

Downloaded using OpenML:
[credit-g](https://www.openml.org/d/31)
