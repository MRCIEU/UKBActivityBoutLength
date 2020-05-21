

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
        # 1-15 minutes
        # 16-40
	# 41+

	# strata of activity level
	# 100 - 125
	# 126 - 150
	# 151 +

	al1 = 125
	al2 = 150

	# check that bouts are always AUC>=100
	ix0 = which(bouts$auc <= 100)
#	print('bouts < 100mg')
#	print(length(ix0))


	ix11 = which(bouts$duration <= 15 & bouts$auc <= al1)
	ix12 = which(bouts$duration <= 15 & bouts$auc > al1 & bouts$auc <= al2)
	ix13 = which(bouts$duration <= 15 & bouts$auc > al2)

	ix21 = which(bouts$duration > 15 & bouts$duration <= 40 & bouts$auc <= al1)
        ix22 = which(bouts$duration > 15 & bouts$duration <= 40 & bouts$auc > al1 & bouts$auc <= al2)
        ix23 = which(bouts$duration > 15 & bouts$duration <= 40 & bouts$auc > al2)

	ix31 = which(bouts$duration > 40 & bouts$auc <= al1)
        ix32 = which(bouts$duration > 40 & bouts$auc > al1 & bouts$auc <= al2)
        ix33 = which(bouts$duration > 40 & bouts$auc > al2)

	# derive the time spent in bouts in each strata overall

	dur1_auc1 = sum(bouts$duration[ix11])
	dur1_auc2 = sum(bouts$duration[ix12])
	dur1_auc3 = sum(bouts$duration[ix13])
	dur2_auc1 = sum(bouts$duration[ix21])
        dur2_auc2 = sum(bouts$duration[ix22])
        dur2_auc3 = sum(bouts$duration[ix23])
	dur3_auc1 = sum(bouts$duration[ix31])
        dur3_auc2 = sum(bouts$duration[ix32])
        dur3_auc3 = sum(bouts$duration[ix33])

	n_11 = length(ix11)
	n_12 = length(ix12)
	n_13 = length(ix13)
	n_21 = length(ix21)
	n_22 = length(ix22)
	n_23 = length(ix23)
	n_31 = length(ix31)
	n_32 = length(ix32)
	n_33 = length(ix33)
	return(data.frame(dur1_auc1, dur1_auc2, dur1_auc3, dur2_auc1, dur2_auc2, dur2_auc3, dur3_auc1, dur3_auc2, dur3_auc3, n_11, n_12, n_13, n_21, n_22, n_23, n_31, n_32, n_33))
	
}
