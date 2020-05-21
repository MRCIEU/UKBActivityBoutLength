

# Run and plot mortality associations



## Mortality analyses

These are all functions in this directory, called from mortalityAnalyses.r


```bash
Rscript mortalityAnalysesHybrid.r
Rscript mortalityAnalysesPredicted.r
```



Person years:

```bash
Rscript	personYears.r
```




## Plot results


Overall time in activity categories, swapping between them:

```bash
matlab -r plotOverall
```

Overall time in activity categories, swapping between them but with MVPA 100mg threshold approach:

```bash
matlab -r plotOverallHybrid
```

Sedentary bout strata:

```bash
matlab -r plotSedentary
```

Sedentary bout strata, with MVPA 100mg threshold approach:

Sedentary bout strata:

```bash
matlab -r plotSedentaryHybrid
```

MVPA bout strata:

Sedentary bout strata:

```bash
matlab -r plotMVPA
```

MVPA bout strata, with MVPA 100mg threshold approach:

```bash
matlab -r plotMVPAHybrid
```


