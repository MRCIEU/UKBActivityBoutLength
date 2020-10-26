
getConfounderVariablesAll <- function(data) {

	confs = getConfounderVariables(data)

	## add bmi and number of illnesses
	confs = cbind(confs, data[,c('bmi', 'prev_firstocc_circ', 'prev_firstocc_resp', 'prev_firstocc_canc')])

	return(confs)
}
