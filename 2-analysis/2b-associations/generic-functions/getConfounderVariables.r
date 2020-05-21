
# like getConfounders.r but sets some of the indicator variables to NULL
# for use in regressions so it isn't over specified

getConfounderVariables <- function(data) {


	confs = getConfounders(data)

	# remove variables where they would be overspecified
	confs$ethwhite = NULL
	confs$winter = NULL
	confs$ednone = NULL

	return(confs)

}
