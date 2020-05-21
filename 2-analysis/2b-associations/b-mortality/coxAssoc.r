




coxAssoc <- function(survivalStartAge, survivalEndAge, survivalStatus, indepVars, resfile, baselineName, adj) {

	print(paste0('********* Baseline category: ', baselineName))

	# delayed entry / left truncation (this is not the same as left censoring which is when they have died before)

	res.cox <- coxph(Surv(survivalStartAge, survivalEndAge, survivalStatus) ~., data = indepVars)

	sumx = summary(res.cox)
	print(sumx)

	print('------- TEST PROP HAZARDS ASSUMPTION -------')
	test.ph <- cox.zph(res.cox, transform='log')
	print(test.ph)
	print('------- -------')



	for (i in 1:nrow(sumx$coef)) {
		rowx = rownames(sumx$coef)[i]
#		print(rowx)

		beta = sumx$coef[rowx, 'coef']
		lower = log(sumx$conf.int[rowx, 'lower .95'])
		upper = log(sumx$conf.int[rowx, 'upper .95'])
		pvalue = signif(sumx$coef[rowx, 'Pr(>|z|)'], digits=2)

		schoenfieldP = test.ph$table[rowx, 'p']

		## scale time estimates to be in terms of 10 minutes of time rather than 1 minute
		if (startsWith(rowx, 'overall') | startsWith(rowx, 'dur')) {
			print(paste0('Scaling estimate to be in terms of 10-minutes of time: ', rowx))
			beta = beta*10
			lower = lower*10
			upper = upper*10
		}
		else {
			print(paste0('Not scaling estimate: ', rowx))
		}


		estimateStr = sprintf(fmt="%.3f [%.3f; %.3f]", beta, lower, upper)
		resline = paste(baselineName, rowx, 'cox-log', estimateStr, beta, lower, upper, sprintf(fmt="%.3f", pvalue), sprintf(fmt="%.3f", schoenfieldP), adj, sep=',')
		print(resline)
		write(resline, file=resfile, append='TRUE')

		estimateStr = sprintf(fmt="%.3f [%.3f; %.3f]", exp(beta), exp(lower), exp(upper))
		resline = paste(baselineName, rowx, 'cox-hr', estimateStr, exp(beta), exp(lower), exp(upper), sprintf(fmt="%.3f", pvalue), sprintf(fmt="%.3f", schoenfieldP), adj, sep=',')
		print(resline)
		write(resline, file=resfile, append='TRUE')

	}

}
