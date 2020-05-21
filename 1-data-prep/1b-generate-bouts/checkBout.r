checkBout <- function(timeSeries, epochLength) {


        validBout = TRUE

	if (nrow(timeSeries)>1) {
	        # check all time differences between epochs are epochLength
		seq1 = timeSeries$time[1:(length(timeSeries$time)-1)]
		seq2 = timeSeries$time[2:length(timeSeries$time)]
		diffs = as.numeric(difftime(seq2, seq1, units="secs"))
		diffsWrong = which(diffs!=epochLength)
		if (length(diffsWrong)>0) {
			validBout = FALSE
		}
	}

	# check all epochs are on valid day
	notValid = which(timeSeries$validDay == FALSE)
	if (length(notValid)>0) {
		validBout = FALSE
	}

        return(validBout)

}

