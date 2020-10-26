

# pid = participant id
# imputed = imp or cd
# thresh = 3mets or 100
aggregateBoutsForPerson <- function(bouts, daytimeonly=FALSE) {
#	print(daytimeonly)

	if (daytimeonly == TRUE) {

		print('xxxxxxxxx')	
		print(sum(bouts$duration))

		# convert so mday etc work
		bouts$startdate = as.POSIXlt(bouts$startdate)
		bouts$enddate = as.POSIXlt(bouts$enddate)

		# remove all those crossing days - i.e. having a different date for startdate and enddate
#		ix = which(bouts$startdate$mday==bouts$enddate$mday)
#		bouts = bouts[ix,]
#		print(unique(bouts$startdate$hour))

		## both between 8am and 10pm on the same day
		ix1 = which(bouts$startdate$mday==bouts$enddate$mday & bouts$startdate$hour < 22 & bouts$startdate$hour >=8 & bouts$enddate$hour< 22 & bouts$enddate$hour >=8)

		## same day crossing over 22:00
		ix2 = which(bouts$startdate$mday==bouts$enddate$mday & bouts$startdate$hour < 22 & bouts$startdate$hour >=8 & bouts$enddate$hour >= 22)
		if (length(ix2)>0) {
		print('same day crossing over 22:00')
		print(bouts[ix2,])
		# adjust values for part of this bout
		bouts$duration[ix2] = (60*(22-bouts$startdate[ix2]$hour) - bouts$startdate[ix2]$min+1)
		print(bouts[ix2,])
		}

		## same day crossing over 08:00
		ix3 = which(bouts$startdate$mday==bouts$enddate$mday & bouts$startdate$hour < 8	& bouts$enddate$hour >= 8 & bouts$enddate$hour < 22)
		if (length(ix3)>0) {
		print('same day crossing over 08:00')
		print(bouts[ix3,])
		# adjust values for part of this bout
                bouts$duration[ix3] = (60*(bouts$enddate[ix3]$hour - 8) + (bouts$enddate[ix3]$min+1))
		print(bouts[ix3,])
		}

		## diff days start is valid but end is early morning
		ix4 = which(bouts$startdate$mday!=bouts$enddate$mday & bouts$startdate$hour>=8 & bouts$startdate$hour < 22 & bouts$enddate$hour < 8)
		if (length(ix4)>0) {
		print('diff days start is valid but end is early morning')
		print(bouts[ix4,])
		bouts$duration[ix4] = (60*(22-bouts$startdate[ix4]$hour) - bouts$startdate[ix4]$min+1)
		print(bouts[ix4,])
		}

		## diff	days end is valid but start is early morning
		ix5 = which(bouts$startdate$mday!=bouts$enddate$mday & bouts$startdate$hour < 8 & bouts$enddate$hour > 8 & bouts$enddate$hour <22)
		if (length(ix5)>0) {
		print('diff days end is valid but start is early morning')
		print(bouts[ix5,])
		bouts$duration[ix5] = (60*(bouts$enddate[ix5]$hour-8) + (bouts$enddate[ix5]$min+1)) 
		print(bouts[ix5,])
		}

		## same	day start early finish late
                ix6 = which(bouts$startdate$mday==bouts$enddate$mday & bouts$startdate$hour < 8 & bouts$enddate$hour >=22)
		if (length(ix6)>0) {
		print('same day start early finish late')
		print(bouts[ix6,])
		bouts$duration[ix6] = 60*14+1
		print(bouts[ix6,])
		}

		# keep all the bouts within 8am - 10pm
                bouts = bouts[c(ix1, ix2, ix3, ix4, ix5, ix6),]

		print(length(c(ix1, ix2, ix3, ix4, ix5, ix6)))
		print(length(unique(c(ix1, ix2, ix3, ix4, ix5, ix6))))

		print(sum(bouts$duration))
		print('zzzzzzzzzzzzz')
	}

	# strata of bout length
        # 1 minute
	# 2-9 minutes
	# 10-15 minutes
        # 16-40 minutes
	# 41+ minutes

	# strata of each bout length
	ix1 = which(bouts$duration <= 9)
	ix2 = which(bouts$duration > 9 & bouts$duration <= 15)
	ix3 = which(bouts$duration > 15 & bouts$duration <= 40)
	ix4 = which(bouts$duration > 40)

	# derive the time spent in bouts in each strata overall

	dur1 = sum(bouts$duration[ix1])
	dur2 = sum(bouts$duration[ix2])
	dur3 = sum(bouts$duration[ix3])
	dur4 = sum(bouts$duration[ix4])

	n_1 = length(ix1)
	n_2 = length(ix2)
	n_3 = length(ix3)
	n_4 = length(ix4)
	
	return(data.frame(dur1, dur2, dur3, dur4, n_1, n_2, n_3, n_4))
	
}
