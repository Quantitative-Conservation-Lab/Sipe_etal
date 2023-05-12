
#### Integrating community science and agency-collected monitoring data to expand monitoring capacity at large spatial scales

Sipe HA, IN Keren, and SJ Converse

Ecosphere

Please contact the first author for questions about the code or data: Hannah Sipe (sipeh@uw.edu). 

####Citation
Sipe HA, IN Keren, and SJ Converse. 2023. Integrating community science and agency-collected monitoring data to expand monitoring capacity at large spatial scales. Ecosphere. 

#### Abstract
Monitoring species to better understand their status, ecology, and management needs is a major expense for agencies tasked with biodiversity conservation. Community science data have the potential to improve monitoring for minimal cost, given appropriate analytical frameworks. We describe a framework for integrating data from the eBird community science platform with agency-collected monitoring data using a multi-state occupancy model. Our model accounts for the structural differences across datasets and allows for estimation of both occupancy and breeding probabilities. The framework was applied to Common Loons (Gavia immer) in Washington State. A total of 766 sites had observation effort; 713 sites had only eBird effort, 26 sites had only Washington Department of Fish and Wildlife (WDFW) effort, and 27 sites had both. We predicted that the probability of occupancy was only 0.07 (95% Bayesian credible interval, BCI = 0.02, 0.51) at the 2324 sites in our sampling frame, though the probability that Common Loons were breeding at occupied sites was 0.95 (95% BCI = 0.71, 1.00). We found that probability of occupancy was positively related to waterbody size (probability of a positive effect = 0.88) and negatively related to an index of human influence (probability of a negative effect = 0.94). We found that probability of breeding at occupied sites was positively related to tree canopy cover (0.86), negatively related to elevation (0.99), and negatively related to barren, scrub/shrub, and herbaceous land cover (0.98).We found that state agency biologists were 16 times more likely to detect breeding Common Loons at a site than were eBird users (0.94, 95% BCI = 0.78, 0.99 for agency biologists vs. 0.08, 95% BCI = 0.06, 0.10 for eBird users). However, the amount of effort expended by eBird users meant that they confirmed Common Loons at 94 sites while agency biologists confirmed them at just 24 sites, although evidence of reproduction was only contributed by agency biologists. Our results provide a better understanding of the distribution of Common Loons in Washington, while further demonstrating that community science data can be a valuable complement to agency-collected data, if appropriate frameworks are developed to integrate these data sources. 

#### Contents of this repository

1.  Data Folder: Description text file with eBird data access query information and         links to download site covariate data for use in ArcMap. Washington Department of       Fish and Wildlife Common Loon observation data from 2017. SitesandCovs.csv data file     with all sites and covariate information, including indicators for data source          (eBird or WDFW). See DESCRIPTION.txt for more information and links to covariate        data sources.

2.  Scripts Folder: R scripts for filtering eBird data to map in ArcMap,
    formatting spatially filtered eBird data and WDFW data for use in
    the occupancy model, Nimble multi-state occupancy model script to run
    the model, and files to generate figures and parameter estimates

3.  Figures Folder:Figures 1-4 from the manuscript, folder with posterior coefficient       density plots from MCMC output

**Scripts**

1.  EBird_Auk_filtering.R & WDFWformat.R

2.  SPfilteredEBird.R & FormattedData.R

3.  NimbleOccModel.R

4.  PosteriorsandTables.R, PredictionsandFigs.R, & ConvergeDiag.R

**Figures** 

Figures found in manuscript Folder with posterior estimates
from coefficients and key parameters

### Required Packages

nimble

auk

coda

ggplot2

devtools

dplyr

data.table

lubridate

bayesplot

cowplot
