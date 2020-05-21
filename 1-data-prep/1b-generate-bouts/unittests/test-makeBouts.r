library("testthat")
source('../makeBouts.r')
source('../checkBout.r')
source('genTestTimeSeq.r')


## Test 1

validDay=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
avm=c(1,2,3,4,5,6,7,8,9,10)
time=genTestTimeSeq(10)
dayidx=c(1,1,1,1,1,1,1,1,1,1)
seq = data.frame(validDay, avm, time, dayidx)
seq$imputed=FALSE
bouts = makeBouts(seq, 60)

expect_equal(length(bouts), 0)
print('test 1 OK')


## Test 2

validDay=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
avm=c(1,2,3,100,5,6,7,8,9,10)
dayidx=c(1,1,1,1,1,1,1,1,1,1)
seq = data.frame(validDay, avm, time, dayidx)
seq$imputed=FALSE
bouts = makeBouts(seq,60)
boutsCorrect = data.frame(index=0, duration=60, auc=100/60, validBout=TRUE, partial=FALSE, startTime=seq$time[4], endTime=seq$time[4], boutContainsImputeBound=FALSE, edgeImputeBound=FALSE)

expect_equal(bouts, boutsCorrect)
print('test 2 OK')


## Test 3

validDay=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
avm=c(1,2,3,100,110,6,7,8,9,10)
dayidx=c(1,1,1,1,1,1,1,1,1,1)
seq = data.frame(validDay, avm, time, dayidx)
seq$imputed=FALSE
bouts = makeBouts(seq,60)

boutsCorrect = data.frame(index=0, duration=120, auc=210/120, validBout=TRUE, partial=FALSE, startTime=seq$time[4], endTime=seq$time[5], boutContainsImputeBound=FALSE, edgeImputeBound=FALSE)

expect_equal(bouts, boutsCorrect)
print('test 3 OK')


## Test 4

validDay=c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE)
avm=c(1,2,3,100,110,6,7,8,9,10)
dayidx=c(1,1,1,2,3,3,3,3,3,3)
seq = data.frame(validDay, avm, time, dayidx)
seq$imputed=FALSE
bouts = makeBouts(seq,60)

expect_equal(length(bouts), 0)
print('test 4 OK')


## Test 5

validDay=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
avm=c(1,2,3,100,110,6,7,8,9,105)
dayidx=c(1,1,1,1,1,1,1,1,1,1)
seq = data.frame(validDay, avm, time, dayidx)
seq$imputed=FALSE
bouts = makeBouts(seq,60)

boutsCorrect = data.frame(index=c(0,1), duration=c(120,60), auc=c(210/120,105/60), validBout=c(TRUE,TRUE), partial=c(FALSE, TRUE), startTime=seq$time[c(4,10)], endTime=seq$time[c(5, 10)], boutContainsImputeBound=c(FALSE, FALSE), edgeImputeBound=c(FALSE, NA))

expect_equal(bouts, boutsCorrect)
print('test 5 OK')





