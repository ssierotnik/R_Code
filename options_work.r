## demonstration with options data
## get data, manipulate data, price option, calculate greeks
require('xts')
require('quantmod')
require('functional')

## get data. using quantmod function which returns an R list with three objects: calls, puts, and symbol
spy.opt = getOptionChain("SPY")
df.call = spy.opt$calls
df.put = spy.opt$puts
df.call = subset(df.call, OI > 200 & Bid > 0 & Ask > 0) #filtering to more liquid strikes
df.put = subset(df.put, OI > 200 & Bid > 0 & Ask > 0)	
df.call['type'] = 'call' #add field to distinguish calls and puts
df.put['type'] = 'put'
df.options =  rbind(df.call[c('Strike','Bid','Ask','type')],df.put[c('Strike','Bid','Ask','type')])
df.options['Mid'] = (df.options['Bid'] + df.options['Ask']) / 2	#create a mid price. this is what we'll use for computing implied vols and greeks

## Black-Scholes Pricer
bsprice = function(S, K, T, r, v, d, Type)
	{
		
		d1 = (log(S / K) + (r - d + 0.5 * v^2) * T) / (v * (T^0.5))
		d2 = d1 - v * (T^0.5)
		if(Type == "call"){
			price = exp(-d*T)*S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
		}
		else if(Type == "put"){
			price = K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
		}
		else{
			price = -100 #putting something here to flag options with no type
		}
		return(price)
	}

#implied vol solver
bsvol = function(S, K, T, r, d, Price_Mkt, Type)
	{
		bspxdiff = function(S, K, T, r,v=.2, d, Price_Mkt, Type)
		{
			Price_Mkt - bsprice(S, K, T, r, v, d, Type)
		}
		bspxdiff.sig = function(v) bspxdiff(S, K, T, r,v, d, Price_Mkt, Type) #similar to the @variable evaluation within matlab
		impliedvol = uniroot(bspxdiff.sig,c(.01,.99),tol= 1e-15)$root
		return(impliedvol)
	}

## calculate greeks
##delta
bsdelta = function(S, K, T, r, v, d, Type)
	{
		d1 = (log(S / K) + (r - d + 0.5 * v^2) * T) / (v * (T^0.5))
		if(Type == "call"){
			delta = pnorm(d1)
		}
		else if(Type == "put"){
			delta = pnorm(d1) - 1
		}
		else{
			delta = NaN #putting something here to flag options with no type
		}
		return(delta)
	}

##gamma
bsgamma = function(S, K, T, r, v, d)
	{
		d1 = (log(S / K) + (r - d + 0.5 * v^2) * T) / (v * (T^0.5))
		nd1 = exp(-0.5*(d1^2)) / (sqrt(2 * pi))
		gam = nd1 / (S*v*sqrt(T))
		return(gam)
	}

##vega
bsvega = function(S, K, T, r, v, d)
	{
		d1 = (log(S / K) + (r - d + 0.5 * v^2) * T) / (v * (T^0.5))
		nd1 = exp(-0.5*(d1^2)) / (sqrt(2 * pi))
		vega = S*nd1*sqrt(T)
		return(vega/100)
	}

##theta
bstheta = function(S, K, T, r, v, d, Type)
	{
		d1 = (log(S / K) + (r - d + 0.5 * v^2) * T) / (v * (T^0.5))
		d2 = d1 - v * (T^0.5)
		nd1 = exp(-0.5*(d1^2)) / (sqrt(2 * pi))
		nd2 = pnorm(d2)
		if(Type == "call"){
			theta = -((S*nd1*v)/(2*sqrt(T)) - r*K*exp(-r*T)*nd2)
		}
		else if(Type == "put"){
			theta = -((S*nd1*v)/(2*sqrt(T)) - r*K*exp(-r*T)*(1-nd2))
		}
		else{
			theta = NaN 
		}
		return(theta/252)
	}

##rho
bsrho = function(S, K, T, r, v, d, Type)
	{
		d1 = (log(S / K) + (r - d + 0.5 * v^2) * T) / (v * (T^0.5))
		d2 = d1 - v * (T^0.5)
		nd2 = pnorm(d2)
		if(Type == "call"){
			rho = K*T*exp(-r*T)*nd2
		}
		else if(Type == "put"){
			rho = -K*T*exp(-r*T)*(1-nd2)
		}
		else{
			rho = NaN #putting something here to flag options with no type
		}
		return(rho/10000)
	}
	