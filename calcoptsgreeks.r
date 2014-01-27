##take in data frame of prices
##compute bs price
##compute ivol and greeks. store ivol and greeks in a dataframe
##plot

##first function takes in a ticker and retrieves the front-month option chain, using some optional filters
##example: getOptsData.func("SPY",min.OI=500,quotefilter=True) will return a dataframe of calls&puts filtered on:
##the underlying symbol == SPY
##the minimum OI per strike >=500
##the strike having a closing bid and offer price
getOptsData.func = function(symbol,min.OI,quotefilter)
	{
		require('quantmod')
		df.pullopt = getOptionChain(symbol)	#using quantmod's function to grab the data from yahoo
		df.call = df.pullopt$calls
		df.put = df.pullopt$puts
		if(missing(quotefilter))
			{
				df.call = subset(df.call, OI >=min.OI & Bid > 0 & Ask > 0) #filtering to more liquid strikes
				df.put = subset(df.put, OI >= min.OI & Bid > 0 & Ask > 0)
			}
		else if(quotefilter == TRUE)
			{
				df.call = subset(df.call, OI >=min.OI & Bid > 0 & Ask > 0) #filtering to more liquid strikes
				df.put = subset(df.put, OI >= min.OI & Bid > 0 & Ask > 0)
			}
		else if(quotefilter == FALSE)
			{
				df.call = subset(df.call, OI >=min.OI) 
				df.put = subset(df.put, OI >= min.OI)
			}
		
		df.call['type'] = 'call' #add field to distinguish calls and puts
		df.put['type'] = 'put'
		df.options =  rbind(df.call[c('Strike','Bid','Ask','type')],df.put[c('Strike','Bid','Ask','type')])
		df.options['Mid'] = (df.options['Bid'] + df.options['Ask']) / 2	#create a mid price. this is what we'll use for computing implied vols and greeks
		
		return(df.options)
	}

calcgreeks = function(S,K,T,r,d,Price_Mkt,Type)
	{
		#K = vector/dataframe of strikes
		#Price_Mkt = vector/dataframe of market option prices
		#Type = vector/dataframe of option type; i.e.: "call" or "put"
		#e.g.
		#                     Strike   Bid   Ask type    Mid
		#AAPL140131C00500000  500.0 47.65 48.35 call 48.000
		#output:
		#implied vol, delta, gamma, vega, theta, rho
		len	=	length(K)
		df.greeks = as.data.frame(matrix(nrow=len,ncol=6))
		names(df.greeks)	=	c('ivol','delta','gamma','vega','theta', 'rho')
		
		i = 1
		while(i <= len)
			{
				df.greeks$ivol[i] 	= 	bsvol(S, K[i], T, r, d, Price_Mkt[i], Type[i])
				v					=	df.greeks$ivol[i]	#implied vol
				df.greeks$delta[i] 	= 	bsdelta(S, K[i], T, r, v, d, Type[i])
				df.greeks$gamma[i] 	= 	bsgamma(S, K[i], T, r, v, d)
				df.greeks$vega[i]	=	bsvega(S, K[i], T, r, v, d)
				df.greeks$theta[i] 	= 	bstheta(S, K[i], T, r, v, d, Type[i])
				df.greeks$rho[i]	=	bsrho(S, K[i], T, r, v, d, Type[i])
				i = i + 1
			}
		return(df.greeks)
	}
				