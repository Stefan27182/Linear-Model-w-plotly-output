
## Brief project with linear model used to find connections between variables

#### This project uses a linear model to find connections between variables, in this case Crude Oil, Silver, DJIA, Shenzhen, E-Mini S&P 500, & TSE.

#### All NA's, dashes, and null's are replaced by a 5x5 moving average, and MICE scans the 10 rows missed by the moving average.

#### Statistically significant connections were found between E-Mini, DJIA, TSE, and Shenzhen.

#### This project uses a standard R project, as opposed to an R markdown project, and outputs its plots as both standard plots, saved as png's, and plotly plots that are saved as html documents. 