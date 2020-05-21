
# our hybrid approach assigns bouts with >100mg as MVPA, then assigns timepoints not assigned as MVPA
# to bouts of other activity types (sleep, sedentary and light)
# this code removes the 100mg periods from these other bouts, since there could (and is) overlap between those minutes assigned as sleep, sed or light, and those with activity >100mg.

exclude100mg <- function(bouts, bouts100) {


	boutsNo100mg = data.frame(boutidx=numeric(), duration_secs=numeric(), auc=numeric(), complexity=numeric(), class=character(), valid=logical(), partial=logical(), startdate=as.Date(character()), enddate=as.Date(character()), boutContainsImputeBound=logical(), edgeImputeBound=logical())


	# for each bout check it doesn't contain a bout with activity >100mg

	for (i in 1:nrow(bouts)) {

		currentBout = bouts[i,]

		# there is some part of a 100mg bout that is within the current bout
		#ix = which(bouts100$startdate <= currentBout$enddate & bouts100$enddate >= currentBout$startdate)

		###
		### there are four scenarios we need to deal with
		
		### 1. current bout is completely contained in a 100mg bout
		currentBout = boutContainsAll(bouts100, currentBout)
		
		# 100mg bout does completely contain currentBout so we don't include the current bout in the new bout list
		if (is.null(currentBout)) {

#			print('complete bout coverage')

			next
		}

		### 2. current bout has a 100mg bout intersecting at the start - this bit of the bout is removed
		currentBout = boutIntersectStart(bouts100, currentBout)

		### 3. current bout has a 100mg bout instersecting at the end - this bit of the bout is removed
		currentBout = boutIntersectEnd(bouts100, currentBout)


		### 4. current bout contains a 100mg bout, so it needs splitting into multiple bouts either side of the 100mg bout(s)
		# there can be several of this type
	
                ix = which(bouts100$startdate >= currentBout$startdate & bouts100$enddate <= currentBout$enddate)

		if (length(ix) == 0) {
			
			# current bout is valid (not containing 100mg bout), so add to out new list

			boutsNo100mg = rbind(boutsNo100mg, currentBout)

		}
		else  {
			# there are some 100mg bouts in this currentBout

			for (j in ix) {
				current100 = bouts100[j,]

				newBoutBefore = currentBout
				newBoutBefore$enddate =  as.POSIXlt(current100$startdate-60)
				newBoutBefore$duration_secs = as.numeric(difftime(newBoutBefore$enddate, newBoutBefore$startdate, units="secs")) + 60
				newBoutBefore$duration =  newBoutBefore$duration_secs/60

				# aucs are unused in category bouts
				#newBoutBefore$auc = currentBout$AUC

				# add bout before 100mg bout
				boutsNo100mg = rbind(boutsNo100mg, newBoutBefore)

				currentBout$startdate = as.POSIXlt(current100$enddate+60)
				currentBout$duration_secs = as.numeric(difftime(currentBout$enddate, currentBout$startdate, units="secs")) + 60
				currentBout$duration = currentBout$duration_secs/60

				# aucs are unused in category bouts
				#newBoutAfter$auc = currentBout$auc 

                        }


			# there must be a bout after the last 100mg block since otherwise it would be dealt with in the boutIntersectEnd function, so add this last bout now
			boutsNo100mg = rbind(boutsNo100mg, currentBout)
		}


	# end for loop
	}

	return(boutsNo100mg)

}

# 100mg bout starts before and ends within current bout - there can be only 1 of this type
boutIntersectStart <- function(bouts100, currentBout) {

	ix = which(bouts100$startdate <= currentBout$startdate & bouts100$enddate >= currentBout$startdate)

	if (length(ix)>=1) {
		if (length(ix)>=2) {
			stop('error: can not be more than 1 100mg bout intersecting the start of the current bout')
		}
		else if (length(ix)==1) {
			# 100mg bout intersecting start of current bout found

			# start time is minute after 100mg bout end
			currentBout$startdate = as.POSIXlt(bouts100$enddate[ix]+60)

			# plus 60 because e.g. if end of 100mg is 1 epoch after start of current bout then they overlap by 2 epochs
			numSecs	= as.numeric(difftime(currentBout$enddate, currentBout$startdate, units="secs")) + 60
                        currentBout$duration_secs = numSecs
			currentBout$duration = currentBout$duration_secs/60

			# aucs are unused in category bouts
			#currentBout$auc = currentBout$auc

		}
	}

	return(currentBout)

}



# 100mg bout starts before end of current bout and finishes after - there can be only 1 of this type
boutIntersectEnd <- function(bouts100, currentBout) {

	#ix = which(bouts100$startdate <= currentBout$enddate & bouts100$enddate > currentBout$enddate)
	ix = which(bouts100$startdate <= currentBout$enddate & bouts100$enddate >= currentBout$enddate)

	if (length(ix)>=1) {
		if (length(ix)>=2) {
			stop('error: can not be more than 1 100mg bout intersecting the start of the current bout')
		}
		else if (length(ix)==1) {

			 # end time is minute before 100mg bout start
			currentBout$enddate = as.POSIXlt(bouts100$startdate[ix]-60)

			numSecs = as.numeric(difftime(currentBout$enddate, currentBout$startdate, units="secs")) + 60
			currentBout$duration_secs = numSecs
			currentBout$duration = currentBout$duration_secs/60

			# aucs are unused in category bouts
			#currentBout$auc = currentBout$auc

		}
	}

	return(currentBout)
}

# there is a bout100 that contains the whole currentBout
boutContainsAll <- function(bouts100, currentBout) {
	
	ix = which(bouts100$startdate <= currentBout$startdate & bouts100$enddate >= currentBout$enddate)

	if (length(ix)>=1) {

		# completely contains
		return(NULL)
	}

	else {
		return(currentBout)
	}

}
