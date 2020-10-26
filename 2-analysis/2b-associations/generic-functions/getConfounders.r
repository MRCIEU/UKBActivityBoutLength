

getConfounders <- function(data) {

	# sex: 31
	# age: age at accelwearstart
	# ethnicity: 21000
	# education: 6138
	# area-based socioeconomic position: 189

	conf = data[,c('eid', 'sex', 'survivalStartAge', 'townsend', 'income', 'smokestatus')]


	##
	## make ethnicity indicator variables

	### we don't add white indicator so not overspecified

	conf$ethwhite = NA
        ix = which(data$ethnicity >0)
        conf$ethwhite[ix] = 0
        ix = which(data$ethnicity == 1001 | data$ethnicity == 1002 | data$ethnicity == 1003 | data$ethnicity == 1)
        conf$ethwhite[ix] = 1

	# black or black british
	conf$ethblack = NA
	ix = which(data$ethnicity >0)
	conf$ethblack[ix] = 0
	ix = which(data$ethnicity == 4001 | data$ethnicity == 4002 | data$ethnicity == 4003 | data$ethnicity == 4)
	conf$ethblack[ix] = 1

	# asian or asian british
	conf$ethasian = NA
	ix = which(data$ethnicity >0)
	conf$ethasian[ix] = 0
	ix = which(data$ethnicity == 3001 | data$ethnicity == 3002 | data$ethnicity == 3003 | data$ethnicity == 3004 | data$ethnicity == 3)
	conf$ethasian[ix] = 1

	# other
	conf$ethother = NA
	ix = which(data$ethnicity >0)
	conf$ethother[ix] = 0
	ix = which(data$ethnicity == 2001 | data$ethnicity == 2002 | data$ethnicity == 2003 | data$ethnicity == 2004 | data$ethnicity == 5 | data$ethnicity == 6 | data$ethnicity == 2)
	conf$ethother[ix] = 1;

	##
	## make education indicator variables 

	ednames = c('college', 'alevels', 'gcse', 'cse', 'nvq', 'other_profes')
	
	# education is a categorical multiple so we generate one binary variable for each education type
	for (i in 1:6) {
		colname = paste('ed', i, ednames[i], sep='')
		conf[,colname] = NA
		ix = which(data$education0 >0 | data$education0 == -7)
		conf[ix,colname] = 0
		ix = which(data$education0 == i | data$education1 == i | data$education2 == i | data$education3 == i | data$education4 == i | data$education5 == i)
		conf[ix,colname] = 1
	}

	# education "none of the above"
	conf$ednone = NA
       	ix = which(data$education0 >0 | data$education0 == -7)
        conf$ednone[ix] = 0
        ix = which(data$education0 == -7)
        conf$ednone[ix] = 1


	# get day of the year
	data$acceldayofyear <- as.numeric(strftime(data$accelwearstartdate, format = "%j"))
	print(paste0('day of year. min:', min(data$acceldayofyear), ' max: ', max(data$acceldayofyear)))

	conf$seasonCos = cos(2*pi*data$acceldayofyear/365)
	conf$seasonSin = sin(2*pi*data$acceldayofyear/365)

	
        conf$eid = NULL

        return(conf)


}


getSeasonIndicators <- function(data) {


	# seasons

	# get the month and date they started to wear the accelerometer
	library(data.table)
	data$accelwearstartdate = as.POSIXlt(data$accelwearstartdate)
	data$wearDate = data$accelwearstartdate$mday	
	data$wearMonth = month(data$accelwearstartdate)

	# winter: dec - feb
	ix = which(data$wearMonth == 12 | data$wearMonth == 1 | data$wearMonth == 2)
	data$winter = 0
	data$winter[ix] = 1

	# spring: march - may
	ix = which(data$wearMonth == 3 | data$wearMonth == 4 | data$wearMonth == 5)
        data$spring = 0
        data$spring[ix] = 1

	# summer: jun - aug
	ix = which(data$wearMonth == 6 | data$wearMonth == 7 | data$wearMonth == 8)
        data$summer = 0
        data$summer[ix] = 1

	# we don't use an indicator for autumn so it's not overspecified
	# autumn: sept - nov
	ix = which(data$wearMonth == 9 | data$wearMonth == 10 | data$wearMonth == 11)
        data$autumn = 0
        data$autumn[ix] = 1


	# don't assume all participants have worn a accel
	data$winter[which(is.na(data$wearMonth))] = NA
	data$summer[which(is.na(data$wearMonth))] = NA
	data$spring[which(is.na(data$wearMonth))] = NA
	data$autumn[which(is.na(data$wearMonth))] = NA

	return(data)

}
