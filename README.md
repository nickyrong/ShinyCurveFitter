# ShinyCurveFitter
Curve fitting made easy!

## Method
The core of the fitting function comes from R package [lmom](https://cran.r-project.org/web/packages/lmom/index.html) by J. R. M. Hosking. Linear moments of the sample data are calculated and distribution parameters are estimated from the calculated linear moments.

Some of the code were taken from [ShinyCFA](https://github.com/nickyrong/ShinyCFA)

## Usage
The file upload takes either CSV or Excel formats. In either case, only the **first column** will recognized. The imported file should **NOT** have header.

Curve goodness-of-fit (GoF) is currently assessed qualitatively (aka "eye balling") by users. Quantitative GoF possible in future development?

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/) and absolutely no warranty, use at your own risk.

