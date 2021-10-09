#install MICE
install.packages("mice")
library(mice)
md.pattern(data)

install.packages("VIM")
library(VIM)
options(repr.plot.width = 7, repr.plot.height = 3)

mice_plot <- aggr(data, col=c('navyblue','red'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(data), cex.axis=.8,
                    gap=3, ylab=c("Missing data","Pattern"))


aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.9, gap=3, ylab=c("Histogram of missing data","Pattern"))

imputed_Data <- mice(data, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

impute_arg$imputed$Sepal.Length



mi
mi (Multiple imputation with diagnostics) package provides several features for dealing with missing values. Like other packages, it also builds multiple imputation models to approximate missing values. And, uses predictive mean matching method.

#install package and load library
install.packages("mi")
library(mi)



#imputing missing value with mi
mi_data <- mi(data, seed = 335)
