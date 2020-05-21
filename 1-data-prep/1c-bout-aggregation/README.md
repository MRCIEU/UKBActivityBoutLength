



## Bout summaries


In `summariseBouts` directory. Summarises the length of bouts of each type, using a subsample of UKB.


```bash
qsub jscd.sh
```



## Bout aggregations


We calculate the number of minutes spent in bouts of each bout length & activity level strata combination:

Bout length: 1-15mins, 16-40mins, 41+mins.
Activity level (avg per minute): 100-125, 126 - 150, 151+


We produce 1 aggregation file for each 1000 part job. Each row has the columns:

1. Participant ID
2. Bout aggregation columns for the 100 mg threshold and predicted activity classes, the total time spent in bouts of this length/intensity across included days:
 - dur1_auc1, dur1_auc2, dur1_auc3, dur2_auc1, dur2_auc2, dur2_auc3, dur3_auc1, dur3_auc2, dur3_auc3
3. Activity summary columns:
 - imputed_inc: Indicates whether 'other day' imputed version is included (has bouts)
 - imputed_hours: num complete days in the imputed data
 - completedays_inc: Indicates whether 'complete days' version is included (has bouts)
 - num_complete_days: num complete days in 'complete days' data (i.e. unimputed complete days)
 - completedays_avm: AVM using out 'complete days' approach
 - imputed_avm: AVM using our 'other day' imputation approach
 - ad_avm: AVM using Doherty's approach (average across all imputed time points using their imputation approach), i.e. not restricting to complete days



```bash
qsub jobAggCD.sh
qsub jobAggI.sh
qsub jobAggCD-hybrid.sh
qsub jobAggI-hybrid.sh
```
