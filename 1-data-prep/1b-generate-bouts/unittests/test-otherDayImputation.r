library("testthat")
source('../otherDayImputation.r')
source('genTestTimeSeq.r')

library(data.table)

## Test 1
# no imputation needed

avm=c(1,2,3,4,5,6,7,8,9,10)
time=genTestTimeSeq(10)
imputed=c(0,0,0,0,0,0,0,0,0,0)
sedentary=c(1,1,0,0,0,0,0,0,0,0)
moderate=c(0,0,1,1,0,0,0,0,0,0)
sleep=c(0,0,0,0,1,1,0,0,0,0)
walking=c(0,0,0,0,0,0,1,1,0,0)
tasks.light=c(0,0,0,0,0,0,0,0,1,1)

seq = data.frame(imputed, avm, time, sedentary, moderate, sleep, walking, tasks.light)
seq$time = as.POSIXlt(seq$time)
seq$imputed=FALSE

timeSeries = otherDayImputation(seq)

expect_equal(timeSeries, seq)
print('test 1 OK')


##################


## Test 2
# some imputation needed and possible

avm=c(1,2,3,4,5,6,7,8,9,10)
time=genTestTimeSeq(10)

imputed=c(0,0,0,0,0,0,0,0,0,0)
sedentary=c(1,1,0,0,0,0,0,0,0,0)
moderate=c(0,0,1,1,0,0,0,0,0,0)
sleep=c(0,0,0,0,1,1,0,0,0,0)
walking=c(0,0,0,0,0,0,1,1,0,0)
tasks.light=c(0,0,0,0,0,0,0,0,1,1)

seq = data.frame(imputed, avm, time, sedentary, moderate, sleep, walking, tasks.light)

# duplicate and set as next day
seq = rbind(seq,seq)
seq$time[11:20] = seq$time[11:20]+60*60*24
seq$imputed[2:4] = 1
seq$avm[11:20] = seq$avm[11:20]+10
seq$time = as.POSIXlt(seq$time)
timeSeries = otherDayImputation(seq)

seqCorrect = seq
seqCorrect$avm[2:4] = seq$avm[12:14]

expect_equal(timeSeries, seqCorrect)
print('test 2 OK')


##################


## Test 3
# some imputation needed and not possible

avm=c(1,2,3,4,5,6,7,8,9,10)
time=genTestTimeSeq(10)

imputed=c(0,0,0,0,0,0,0,0,0,0)
sedentary=c(1,1,0,0,0,0,0,0,0,0)
moderate=c(0,0,1,1,0,0,0,0,0,0)
sleep=c(0,0,0,0,1,1,0,0,0,0)
walking=c(0,0,0,0,0,0,1,1,0,0)
tasks.light=c(0,0,0,0,0,0,0,0,1,1)

seq = data.frame(imputed, avm, time, sedentary, moderate, sleep, walking, tasks.light)

# duplicate and set as next day
seq = rbind(seq,seq)
seq$time[11:20] = seq$time[11:20]+60*60*24
seq$imputed[2:4] = 1
seq$imputed[14] = 1
seq$avm[11:20] = seq$avm[11:20]+10
seq$time = as.POSIXlt(seq$time)
timeSeries = otherDayImputation(seq)


seqCorrect = seq
seqCorrect$avm[2:4] = NA
seqCorrect$avm[14] = NA

expect_equal(timeSeries, seqCorrect)
print('test 3 OK')


