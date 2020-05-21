
dataDir = Sys.getenv('PROJECT_DATA')

set.seed(1234)

## load all the included participants


ids = c()

i=1
while (i<=93001) {


	## ids from 3+ valid days bout generation
	fileName1=paste0(dataDir, '/accel/derived/activityBouts/boutssample',i,'/cd-ids-inc.txt')
	ids1 = read.table(fileName1, header=0)
	ids = rbind(ids, ids1)

	i=i+1000

}


## randomly select 1000 ids
randIdx = sample(1:nrow(ids), 1000, replace=FALSE)
idsSelected = ids[randIdx,1]

## save ids

write.table(idsSelected, paste0(dataDir, '/accel/derived/mvpaCompare/ids.csv'), col.names=FALSE, row.names=FALSE)






