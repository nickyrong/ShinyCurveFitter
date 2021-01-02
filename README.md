# ShinyCurveFitter
Curve fitting made easy!

## Frequency Analysis Methods:
Some of the code were taken from [ShinyCFA](https://github.com/nickyrong/ShinyCFA)

The frequency distribution(s) are fitted using L-moments method from R package [lmom](https://cran.r-project.org/web/packages/lmom/index.html) by J. R. M. Hosking. L-moments of the sample data are calculated. Distribution parameters are then estimated from the calculated L-moments.

Return periods of annual maximum discharge are calculated using the Weibull formula: *Tr = (N+1) / m* where *N* is the sample size and *m* is the rank. Probability of Non-exceedance is the inverse of return period: *P = 1 / Tr*. Frequency distribution plots' x-axis has been transformed into probabilistic scale: *η = −log(−log(P)) = −log[−log(1 − 1/Tr)]* where *η* is the reduced variate.

**Further readings:**<br/>
* Hosking, J., & Wallis, J. (1997). *Regional Frequency Analysis: An Approach Based on L-Moments*. Cambridge: Cambridge University Press. doi:10.1017/CBO9780511529443<br/>
* Makkonen, L. (2006). Plotting Positions in Extreme Value Analysis, *Journal of Applied Meteorology and Climatology*, 45(2), 334-340. doi:10.1175/JAM2349.1
<br/>


## Usage
The file upload takes either CSV or Excel formats. In either case, only the **first column** will recognized. The imported file should **NOT** have header.

Curve goodness-of-fit (GoF) is currently assessed qualitatively (aka "eye balling") by users. Quantitative GoF possible in future development?

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
Copyright © 2021 [Nick Rong](https://github.com/nickyrong)

Released under the [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
<br/>
