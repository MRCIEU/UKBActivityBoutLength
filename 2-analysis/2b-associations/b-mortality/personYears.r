


source('../../loadData.r')

options(warn=1)


personYears <- function(version) {

	sink(paste0(Sys.getenv('RES_DIR'), '/mortality-person-years-',version,'.txt'))

	data = loadData(version, TRUE)

	##
	## the number of years between wearing the accelerometer and either dying or being censored

	totalPersonYears = sum(data$survivalTime)

	print(paste0('Total person years: ', totalPersonYears))

	meanPersonYears = mean(data$survivalTime)

	print(paste0('Mean person years: ', meanPersonYears))

	sdPersonYears	= sd(data$survivalTime)
        print(paste0('SD person years: ', sdPersonYears))




	sink()

}


personYears('CD')

personYears('imp')


