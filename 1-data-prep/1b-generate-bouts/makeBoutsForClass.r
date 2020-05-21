
# loops through each timepoint in data argument, and identifies bouts of activity for each activity class
makeBoutsForClass <- function(timeSeries, epochLength) {

	# count of bouts identified so far
	boutCounter=0

	# AD classifier classes: moderate,sedentary,sleep,tasks-light,walking,MET
	bouts = c(index=integer(), duration=double(), auc=double(), activityclass=character())

	startthisidx=min(which(timeSeries$validDay==TRUE))

	timeSeries = timeSeries[startthisidx:nrow(timeSeries),]

	# first bout is always a partial start (i.e. it may not be the true beginning of the bout)
	partialStart=TRUE
	lastBout=FALSE
	prevEpochImputed=-1

	while (lastBout == FALSE) {

		currentClass = getClass(timeSeries[1,])
		partialEnd = FALSE

		## find end of this bout - end of day or non-valid day
		ixall = which(timeSeries[,currentClass]==0 | timeSeries$validDay == FALSE)
		if (length(ixall)==0) {
			lastBout = TRUE
			endthisidx = nrow(timeSeries)
			
			# end of time series so it's a partial end
			partialEnd = TRUE
		}
		else {
			ix = min(ixall)
			endthisidx = ix-1

			# may not have been the true end of the bout since we end due to an in valid day
	                partialEnd = timeSeries$validDay[ix]!=TRUE

		}

		thisBout = timeSeries[1:endthisidx,]

                ## check bout is valid
		validBout = checkBout(thisBout, epochLength)

                        # derive AUC
			currentBoutDuration = epochLength*nrow(thisBout)
                        currentBoutEpochs = thisBout$avm
                        currentBoutAUC = sum(currentBoutEpochs)/currentBoutDuration

			partialThis=FALSE
			if (partialStart == TRUE || lastBout==TRUE || partialEnd == TRUE) {
				partialThis = TRUE
			}

#			print(prevEpochImputed)
#			print(thisBout$imputed)
#			print(timeSeries$imputed[endthisidx+1])

			# if go from imputed to non imputed or vice versa then bout is a bit dodgy
			boutContainsImputeBound = length(unique(na.omit(thisBout$imputed)))==2

			# if bout is imputed but previous/subsequent epochs are not, or vice versa, then set edgeImputeBound to TRUE
	                edgeImputeBound = (prevEpochImputed!=-1 & prevEpochImputed!=thisBout$imputed[1]) | (lastBout==FALSE & timeSeries$imputed[endthisidx+1] != thisBout$imputed[nrow(thisBout)])

#			print(boutContainsImputeBound)
#			print(edgeImputeBound)

                        # add bout to bout list
                        newRow = data.frame(index=boutCounter,duration=currentBoutDuration,auc=currentBoutAUC, activityClass=currentClass, validBout=validBout, partial=partialThis, stringsAsFactors=FALSE, startTime=timeSeries$time[1], endTime=timeSeries$time[endthisidx], boutContainsImputeBound=boutContainsImputeBound, edgeImputeBound=edgeImputeBound)
                        bouts = rbind(bouts, newRow)
                        boutCounter = boutCounter + 1


		if (lastBout == FALSE) {
			ix = endthisidx + 1

			# new bout but continuing along a valid region
			if (timeSeries$validDay[ix]==TRUE) {
				startnextidx = ix
				partialStart=FALSE
			}
			else {
				## finding next region after invalid day so set as partial start
				partialStart=TRUE
	
				# move to next valid day

				timeSeries = timeSeries[ix:nrow(timeSeries),]
	
				# find next valid day
				ixall = which(timeSeries$validDay==TRUE)
	                	if (length(ixall)==0) {
        	        	        lastBout = TRUE
                		}
                		else {
                		      	startnextidx = min(ixall)
                		}
			}

		prevEpochImputed = timeSeries$imputed[startnextidx-1]
		timeSeries = timeSeries[startnextidx:nrow(timeSeries),]
		}

	}

	return(bouts)
}


getClass <- function(timeSeriesRow) {

	timeSeriesRow = timeSeriesRow[,c("moderate","sedentary","sleep","tasks.light","walking")]
	if (sum(timeSeriesRow)!=1) {
		print(timeSeriesRow)
		stop("classes do not sum to one")
	}

	ix = which(timeSeriesRow==1)
	return(colnames(timeSeriesRow)[ix])
}

