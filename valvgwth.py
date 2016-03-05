import pandas as pd
import pandas.io.data as web
import datetime
import numpy as np
symbol_list = ['RPV','RPG' ]

start = datetime.datetime(2010,12,31)
#end = datetime.datetime(2016,02,05)
val = web.DataReader(symbol_list[0], "yahoo", start)
gth = web.DataReader(symbol_list[1], "yahoo", start)
val = val['Adj Close']
gth = gth['Adj Close']
val = pd.DataFrame(val)
gth = pd.DataFrame(gth)
val.columns = ['value']
gth.columns = ['growth']
pxdf = val.join(gth)
pxdf.fillna(method='ffill',inplace=True)
pcdf = pxdf.pct_change()
cpdf= (1+pcdf).cumprod(axis=0)
cumdif = cpdf['growth'] - cpdf['value']
cumdif = pd.DataFrame(cumdif)
cumdif.columns = ['gxv']
cumdif['ma_50'] = pd.rolling_mean(cumdif,50)
cumdif['ma_200'] = pd.rolling_mean(cumdif['gxv'], 200)

cumdif['Bol_upper'] = pd.rolling_mean(cumdif['gxv'], window=50) + 2* pd.rolling_std(cumdif['gxv'], 50, min_periods=50)
cumdif['Bol_lower'] = pd.rolling_mean(cumdif['gxv'], window=50) - 2* pd.rolling_std(cumdif['gxv'], 50, min_periods=50)
cumdif.plot(grid=True, title = 'Growth Less Value')
cumdif.tail(50).plot(grid=True)



np.max(cumdif.head(750)) - np.min(cumdif.head(750))
Out[23]: 0.11789636364996592

np.max(cumdif.tail(50)) - np.min(cumdif.tail(50))
Out[25]: 0.07019682983731101