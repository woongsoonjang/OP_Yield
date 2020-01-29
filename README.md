# OP-Yield in R Shiny

This application is a translation of the OP-Yield Microsoft Excel™ spreadsheet v1.00.4 (Ritchie and Zhang 2018) to `R` & `RStudio`'s `Shiny` interactive web application. The application produces growth and yield prediction for ponderosa pine (*Pinus ponderosa* Lawson & C. Lawson) plantations in northern California. Inspired by Oliver and Powers' original work (1978), several features were fixed and added. For further theoretical background and technical detail for the OP-Yield, please refer to the documentation written by M.W. Ritchie and J. Zhang (2018) from US Forest Service. It is available at [https://www.fs.fed.us/psw/publications/documents/psw_gtr259/](https://www.fs.fed.us/psw/publications/documents/psw_gtr259/).

## Description

Using 14 user-specified values (from the ''Input Values'' tab), the application produced yield summary, growing stock, and precommercial thinning summary tables. Via the `Standview` package (Ritche 2018), the application provides the stand density management diagram and additional stand attribute information. Plots for annual volume increment, stand density, and density distribution by crown class can also be generated. Users can download tables and figures into their devices with various file formats (e.g., csv, xlsx for tables and pdf, png, and tiff for figures).  

## Built with
* R/Rstudio/`Shiny`

## Author
* Woongsoon Jang, University of British Columbia (<woongsoon.jang@ubc.ca>).

## Acknowledgments
* Martin Ritchie, US Forest Service
* Jianwei Zhang, US Forest Service

## References
* Edminster, C.B. 1988. Stand density and stocking in even-aged ponderosa pine
stands. In: Bumgartner, D.M.; Lotan J.E., eds. Ponderosa pine: the species and
its management: symposium proceedings. Vancouver, WA: Washington State
University, Cooperative Extension: 253-260.
* DeMars, D.J.; Barrett, J.W. 1987. Ponderosa pine managed-yield simulator: PPSIM
user's guide. Gen. Tech. Rep. PNW-GTR-203. Portland, OR: U.S. Department of
Agriculture, Forest Service, Pacific Northwest Research Station. 36 p
* MacLean, C.D.; Burger, J.M. 1976. Softwood tree volume equations for major
California species. Res. Note PNW-RN-266. Portland, OR: U.S. Department of
Agriculture, Forest Service, Pacific Northwest Forest and Range Experiment
Station. 12 p.
* Oliver, W.W.; Powers, R.F. 1978. Growth models for ponderosa pine: I. yield of
unthinned plantations in northern California. Res. Pap. PSW-RN-133. Berkeley,
CA: U.S. Department of Agriculture, Forest Service, Pacific Southwest Forest
and Range Experiment Station. 21 p.
* Reineke, L.H. 1933. Perfecting a stand-density index for even-aged forests. Journal
of Agricultural Research. 46(7): 627-638.
* Ritchie, Martin W. 2018. Standview. [https://www.fs.fed.us/psw/tools/standview/](https://www.fs.fed.us/psw/tools/standview/)
* __Ritchie, Martin W.; Zhang, Jianwei. 2018. OP-Yield Version 1.00 user’s guide.
Gen. Tech. Rep. PSW-GTR-259. Albany, CA: U.S. Department of Agriculture,
Forest Service, Pacific Southwest Research Station. 26 p.__
* Wensel, L.C.; Olson, C.M. 1995. Tree volume equations for major California
conifers. Hilgardia. 62(5): 1-11.
