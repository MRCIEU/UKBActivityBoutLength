

# MVPA minute comparison

We assess the degree of concordance between the assignment of minutes as MVPA according to the two approaches.


## 1. Select a random sample of 1000 participants with at least 2 valid days

```bash
Rscript makeSample.r
```

## 2. Infer the number of minutes in each section of the venn

a) We process their accelerometer data to 1 minute epochs

b) We select 1 valid day from each person

c)  We count the number of minutes that are in the venn section:

 - MVPA according to 100mg threshold but not MVPA according to prediction.
 - Not MVPA according to 100mg threshold but is MVPA accoding to prediction.
 - MVPA according to both approaches.
 - Not MVPA according to both approaches.


```bash
qsub jobCompare.sh
```




## 3. Summarise the contingency tables

```bash
matlab -r summariseComparison
```
