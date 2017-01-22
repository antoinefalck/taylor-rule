# Taylor Rule

Quick study about how the Taylor rule fits the real historical federal fund rates.
If you're interested in this simply e-mail me and I will provide you my report (french) : `antoine.falck@gmail.com`

The data analysis and figures were made with `R` under the `3.3.2` version.

## Datas

I use different datas which all are from the official Federal Reserve website https://www.stlouisfed.org. 
See the report for the the exact links.

The different files are :
- `DP_LIVE_19012017220247244.csv` which gives the growth values for different countries on different periods
- `FEDFUNDS.csv` the nominal rates from 1954 to today
- `FPCPITOTLZGUSA.csv` the inflation in the US since 1960
- `GDPC1.csv` the real GDP of the US

## Taylor rule implementation

The implementation is given in the principal file `plot.r` and needs the `zoo` library. The different steps are discribed in the final report.

## Plots

All the plots given by the program `plot.r` are in the folder `plot`. They are also part of the interpretation in the final report.

`RData` is a binary file were all the `R` objects are saved in.
