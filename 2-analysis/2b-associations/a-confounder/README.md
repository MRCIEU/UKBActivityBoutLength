

# Comparison of confounders for participants in vs excluded from our sample


## Generate data 

We create a a boolean flag indicating if someone is:
1. in our sample
2. not in our sample but invited to wear an accelerometer

```bash
Rscript generateConfounderSampleData.r
```

## Compare confounders

```bash
stata -b confounderAssocs.do
```


