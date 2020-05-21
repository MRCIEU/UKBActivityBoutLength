# UK Biobank activity bouts analysis

This repository accompanies the paper:

Millard, LAC, et al. Association of changing physical activity intensity and bout length with mortality: a study of 79,507 participants in UK Biobank, medRxiv, 2020.

## Environment details

I use R v3.5.1, Matlab-r2015a, python v2.7.6, and Java jdk-1.8.0-66.

The accelerometer data is processed using the UK Biobank acclerometer tool [here](https://github.com/activityMonitoring/biobankAccelerometerAnalysis/).


The code uses some environment variables that need to be set in your linux environment.

I set the results, project data and UK Biobank accelerometer package directories temporarily with:

```bash
export RES_DIR="${HOME}/2016-biobank-accelerometer/results/activityBouts"
export PROJECT_DATA="${HOME}/2016-biobank-accelerometer/data"
export UKB_ACCEL_TOOL="${HOME}/2016-biobank-accelerometer/code/biobankAccelerometerAnalysis-master"
```

