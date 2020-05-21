

otherDayImputation <- function(timeSeries) {

	timeSeries$timeOfDay = paste(timeSeries$time$hour, ":", timeSeries$time$min, ":", timeSeries$time$sec, sep='')

	end = FALSE

	# earliest index we are considering
	currentIdx = 1

	while (end == FALSE) {


		## find next missing region (imputed by AD package)

#		nextMissing = which(timeSeries$imputed[currentIdx:nrow(timeSeries)] == 1 | is.na(timeSeries$moderate[currentIdx:nrow(timeSeries)]))
		nextMissing = which(timeSeries$imputed[currentIdx:nrow(timeSeries)] == 1)
		
		if (length(nextMissing)==0) {
			end = TRUE
		}
		else {
			
			startMissingIdx = currentIdx + min(nextMissing) - 1
			
			## find end of missing region
			
#			nextNotMissing = which(timeSeries$imputed[startMissingIdx:nrow(timeSeries)] == 0 & !is.na(timeSeries$moderate[startMissingIdx:nrow(timeSeries)]))
			nextNotMissing = which(timeSeries$imputed[startMissingIdx:nrow(timeSeries)] == 0)
			if (length(nextNotMissing)==0) {
				endMissingIdx = nrow(timeSeries)
				end = TRUE
			}
			else {
				# end is 1 before the next not missing (so it is startidx + nextnotmissing - 1 - 1)
				endMissingIdx = startMissingIdx + min(nextNotMissing) - 2
			}

			## find other day where this region isn't missing
			startTimeOfDay = timeSeries$timeOfDay[startMissingIdx]
			endTimeOfDay = timeSeries$timeOfDay[endMissingIdx]
			lengthMissing = endMissingIdx - startMissingIdx + 1


			## find all the other dates with the same time of day
			ixOther = which(timeSeries$timeOfDay == startTimeOfDay)

			idx=1
			if (length(ixOther)> 0) {

				## find all of these regions with no missingness (imputation by AD tool)
				possibleRegionsList = list()
				for (ixx in ixOther) {
					endseqidx = ixx+lengthMissing - 1

					# end index has to be before end of time series
					if (endseqidx<=nrow(timeSeries)) {

						seq = timeSeries[ixx:endseqidx,]

						## check if has missing bits
						#imissing = which(seq$imputed ==1 | is.na(timeSeries$moderate[currentIdx:nrow(timeSeries)]))
						imissing = which(seq$imputed ==1)
						if (length(imissing) == 0) {

							# we can use this to impute
							possibleRegionsList[[idx]] = seq
							idx = idx + 1
						}					
					}
				}

				## impute with a random region if there are some to choose from
				numOptions = length(possibleRegionsList)				
				if (numOptions>=1) {
					## choose random region and set values in missing region
					randChoice = runif(1, 1, numOptions)
					choiceSeq = possibleRegionsList[[randChoice]]

					timeSeries$avm[startMissingIdx:endMissingIdx] = choiceSeq$avm
					timeSeries$moderate[startMissingIdx:endMissingIdx] = choiceSeq$moderate
					timeSeries$walking[startMissingIdx:endMissingIdx] = choiceSeq$walking
					timeSeries$sedentary[startMissingIdx:endMissingIdx] = choiceSeq$sedentary
					timeSeries$sleep[startMissingIdx:endMissingIdx] = choiceSeq$sleep
					timeSeries$tasks.light[startMissingIdx:endMissingIdx] = choiceSeq$tasks.light
					timeSeries$MET[startMissingIdx:endMissingIdx] = choiceSeq$MET

					timeSeries$imputed[startMissingIdx:endMissingIdx] = 1
				}
				else {
					timeSeries$avm[startMissingIdx:endMissingIdx] = NA
#					timeSeries$imputed[startMissingIdx:endMissingIdx] = 0
				}

			}
			else {
				timeSeries$avm[startMissingIdx:endMissingIdx] = NA
#				timeSeries$imputed[startMissingIdx:endMissingIdx] = 0
			}



			currentIdx = endMissingIdx + 1
		}
	}

	timeSeries$timeOfDay = NULL

	return(timeSeries)

}
