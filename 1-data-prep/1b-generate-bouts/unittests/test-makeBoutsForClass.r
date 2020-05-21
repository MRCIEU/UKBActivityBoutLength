library("testthat")
source('../makeBoutsForClass.r')
source('../checkBout.r')
source('genTestTimeSeq.r')


## Test 1

validDay=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
avm=c(1,2,3,4,5,6,7,8,9,10)
time=genTestTimeSeq(10)
sedentary=c(1,1,0,0,0,0,0,0,0,0)
moderate=c(0,0,1,1,0,0,0,0,0,0)
sleep=c(0,0,0,0,1,0,0,0,0,0)
walking=c(0,0,0,0,0,1,1,1,0,0)
tasks.light=c(0,0,0,0,0,0,0,0,1,1)
dayidx=c(1,1,1,1,1,1,1,1,1,1)

seq = data.frame(validDay, avm, time, sedentary, moderate, sleep, walking, tasks.light, dayidx)
seq$imputed = FALSE
bouts = makeBoutsForClass(seq, 60)

index=c(0,1,2,3,4)
duration=c(120,120,60,180,120)
auc=c(1.5, 3.5, 5, 7, 9.5)
auc = auc/60
activityclass=c('sedentary', 'moderate', 'sleep', 'walking', 'tasks.light')
partial=c(TRUE, FALSE, FALSE, FALSE, TRUE)
validBout=c(TRUE,TRUE,TRUE,TRUE,TRUE)
boutsCorrect = data.frame(index=index, duration=duration, auc=auc, activityClass=activityclass, validBout=validBout, partial=partial, stringsAsFactors=FALSE)

expect_equal(bouts$index, boutsCorrect$index)
expect_equal(bouts$duration, boutsCorrect$duration)
expect_equal(bouts$auc, boutsCorrect$auc)
expect_equal(bouts$activityClass, boutsCorrect$activityClass)
expect_equal(bouts$partial, boutsCorrect$partial)

print('test 1 OK')



## Test 2

### bout goes into not valid region

validDay=c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE)
avm=c(1,2,3,4,5,6,7,8,9,10)
time=genTestTimeSeq(10)
sedentary=c(1,1,0,0,0,0,0,0,0,0)
moderate=c(0,0,0,0,0,0,0,0,0,0)
sleep=c(0,0,1,1,1,1,1,0,0,0)
walking=c(0,0,0,0,0,0,0,1,0,0)
tasks.light=c(0,0,0,0,0,0,0,0,1,1)
dayidx=c(1,1,1,1,1,2,3,3,3,3)

seq = data.frame(validDay, avm, time, sedentary, moderate, sleep, walking, tasks.light, dayidx)
seq$imputed = FALSE
bouts = makeBoutsForClass(seq, 60)

index=c(0,1,2,3,4)
duration=c(120,180,60,60,120)
auc=c(1.5, 4, 7,8, 9.5)
auc = auc/60
activityclass=c('sedentary', 'sleep', 'sleep', 'walking', 'tasks.light')
partial=c(TRUE, TRUE, TRUE, FALSE, TRUE)
validBout=c(TRUE,TRUE,TRUE,TRUE,TRUE)
boutsCorrect = data.frame(index=index, duration=duration, auc=auc, activityClass=activityclass, validBout=validBout, partial=partial, stringsAsFactors=FALSE)

expect_equal(bouts$index, boutsCorrect$index)
expect_equal(bouts$duration, boutsCorrect$duration)
expect_equal(bouts$auc, boutsCorrect$auc)
expect_equal(bouts$activityClass, boutsCorrect$activityClass)
expect_equal(bouts$partial, boutsCorrect$partial)

print('test 2 OK')





