

options(warn=1)

##
## get settings from args

args = commandArgs(trailingOnly=TRUE)

# CD vs imputed
sampleVersion = args[1]

# predicted versus hybrid
approach = args[2]


numx = args[3]

print(sampleVersion)
print(approach)
print(numx)

source('aggregateBouts.r')
source('aggregateBoutsForPerson.r')
source('aggregateBoutsHybrid.r')

# complete days version
if (all(sampleVersion == "CD")) {
	print('Complete days version')
} else {
	print('Other day imputed version')
}

##
## run aggregation

if (all(approach == "hybrid")) {
	print("hybrid approach")
	aggregateBoutsHybrid(sampleVersion, numx)
} else {
	aggregateBouts(sampleVersion, numx)
}
