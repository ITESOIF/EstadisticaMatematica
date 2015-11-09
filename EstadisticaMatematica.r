
# -- ----------------------------------------------------------------------------------------- -- #
# -- ITESO, Universidad Jesuita de Guadalajara ----------------------------------------------- -- #
# -- Ingeniería Financiera - Departamento de Matematicas y Fisica ---------------------------- -- #
# -- Licencia: GNU General Public License ---------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

# -- Funcion de Auto Correlacion y Auto Correlacion Parcial ---------------------''----------- -- #

AutoCorrelation <- function(x, type, LagMax)  {
  ciline <- 2/sqrt(length(x))
  bacf   <- acf(x, plot = FALSE, lag.max = LagMax, type = type)
  bacfdf <- with(bacf, data.frame(lag, acf))
  Sig_nc <- (abs(bacfdf[,2]) > abs(ciline))^2
  bacfdf <- cbind(bacfdf, Sig_nc)
return(bacfdf) }
