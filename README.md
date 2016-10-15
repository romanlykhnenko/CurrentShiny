# CurrentShiny

Shiny application created as an examination for the seminar **Programming with R**

Shiny application consists of two distinct subcomponents: **Option calculator** and **VSTOXX Analysis**. **OptionCalculator** can be used to evaluate Black-Scholes price of the option as well
as Delta, Gamma, Rho and Vega of the option. User also has the possibility to
analyze how a given Greek changes if some market parameters change. Implementation of **OptionCalculator** mainly relies on **class BSworld**, which is realized using **S3 object system**.
The component **VSTOXX Analysis** deals with analysis of VSTOOX index and
options on it. Using this part user can take a look at the average value of
options accros different strikes and maturities. Also user can obtain structure
of implied volatility smiles for a given date as well as structure of option prices.
All packages used in application have been collected in the file packages.R.
These packages must be installed to run application.


Further information can be found in **CurrentShiny.pdf**
