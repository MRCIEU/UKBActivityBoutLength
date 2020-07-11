


getConfounderVariablesAll <- function(data) {

	confs = getConfounderVariables(data)

	## add bmi and number of illnesses
	confs = cbind(confs, data[,c('numillness','numillnesscancer', 'bmi')])

	return(confs)
}
