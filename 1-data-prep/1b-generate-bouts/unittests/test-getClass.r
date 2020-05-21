
library("testthat")
source('../makeBoutsForClass.r')



df = data.frame(moderate=1, sedentary=0, sleep=0, 'tasks.light'=0, walking=0)
thisclass = getClass(df)
expect_equal(thisclass, 'moderate')
print('test 1 OK')

df = data.frame(moderate=0, sedentary=1, sleep=0, 'tasks.light'=0, walking=0)
thisclass = getClass(df)
expect_equal(thisclass, 'sedentary')
print('test 2 OK')


df = data.frame(moderate=0, sedentary=1, sleep=1, 'tasks.light'=0, walking=0)
thisclass = getClass(df)
# this should stop








