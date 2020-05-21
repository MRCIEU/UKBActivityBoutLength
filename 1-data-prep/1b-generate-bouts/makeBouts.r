
# loops through each timepoint in data argument, and identifies bouts of activity above threshold activity level
makeBouts <- function(timeSeries, epochLength) {

	# count of bouts identified so far
	boutCounter=0
	bouts = c(index=integer(), duration=double(), auc=double())

	thresh=100

	lastBout = FALSE
	prevEpochImputed = -1

	while (lastBout == FALSE) {

		partialStart=FALSE
		partialEnd=FALSE

		##
		## get next position where AVM is above threshold - this is the start of the next bout

		ix = which(timeSeries$avm>=thresh & timeSeries$validDay == TRUE)
		if (length(ix)==0) {
			# no epochs above threshold in rest of sequence
			break
		}

		# previous time point wasn't in a valid day but was above 100, so it's a partial start
		if (ix==1 || (timeSeries$validDay[ix-1] == FALSE & timeSeries$avm[ix-1]>=thresh)) {
			partialStart=TRUE
		}
		
		startidx = min(ix)

		timeSeries = timeSeries[startidx:nrow(timeSeries),]
#		currentdayidx = timeSeries$dayidx[1]

		##
		## get next position where AVM is below threshold - this is the idx after the end of this next bout
		## or on new day

		# end of bout is at the epoch before the next epoch below the threshold OR the next epoch for the start of a non-valid day
		ixBelow = which(timeSeries$avm<thresh | timeSeries$validDay == FALSE)
		if (length(ixBelow)==0) {
                        # no end of bout in rest of sequence so end
			
			endidx = nrow(timeSeries)
			lastBout = TRUE
			partialEnd = TRUE
                }
		else {
			endidx = min(ixBelow) - 1

			# next time point is not valid but >100 so would have been part of bout, so it's a partial end
			if (timeSeries$validDay[(endidx+1)]==FALSE & timeSeries$avm[(endidx+1)]>=thresh) {
				partialEnd = TRUE
			}
		}

		##
		## check bout is valid

		validBout = checkBout(timeSeries[1:endidx,], epochLength)

			# number of epochs
			currentBoutDuration = epochLength*endidx

			# derive AUC
			currentBoutTimeSeries = timeSeries[1:endidx,]
			currentBoutEpochs = timeSeries$avm[1:endidx]
                       	currentBoutAUC = sum(currentBoutEpochs)/currentBoutDuration

			partialThis=FALSE
                        if (partialStart == TRUE || lastBout==TRUE || partialEnd == TRUE) {
                                partialThis = TRUE
                        }

#			print('xxx1')
#			print(prevEpochImputed)
#			print(currentBoutTimeSeries$imputed)
#			print(timeSeries$imputed[endidx+1])
#			print('xxx2')

			# if go from imputed to non imputed or vice versa then bout is a bit dodgy
                        boutContainsImputeBound = length(unique(na.omit(currentBoutTimeSeries$imputed)))==2

                        # if bout is imputed but previous/subsequent epochs are not, or vice versa, then set edgeImputeBound to TRUE
                        edgeImputeBound = (prevEpochImputed!=-1 & prevEpochImputed!=currentBoutTimeSeries$imputed[1]) | (timeSeries$imputed[endidx+1] !=currentBoutTimeSeries$imputed[nrow(currentBoutTimeSeries)])

#                        print(boutContainsImputeBound)
#                        print(edgeImputeBound)

			# add bout to bout list
			newRow = data.frame(index=boutCounter,duration=currentBoutDuration,auc=currentBoutAUC, validBout=validBout, partial=partialThis, startTime=timeSeries$time[1], endTime=timeSeries$time[endidx], boutContainsImputeBound=boutContainsImputeBound, edgeImputeBound=edgeImputeBound)
			bouts = rbind(bouts, newRow)

			boutCounter = boutCounter + 1



		## get remaining subsequence of time series, after the end of this bout
		
		startnextidx = endidx+1
		if (startnextidx<= nrow(timeSeries)) {
			prevEpochImputed = timeSeries$imputed[endidx]
			timeSeries = timeSeries[startnextidx:nrow(timeSeries),]
		}
	
	}


	return(bouts)

}
