####################################################################
## Feature Selection

corr.matrix<-cor(data[, sapply(data, is.numeric)],
                 use = "complete.obs", method = "pearson")
options(repr.plot.width = 5, repr.plot.height = 5)
corrplot::corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number",is.corr = T)

## inception date and quote date are highly correlated, therefore one can be removed as they give no further insight
##from num analysis we see that inception date and vehicle type don't have influence on SALE and will therefore be removed


# Remove columns we didn't see influence on SALE from above
data <- data %>%
  dplyr::select(
  #  -VEHICLETYPE, -INCEPTIONDATE,
   # -CATAGE,
    -VEHICLEVALUE
    
  )



# Calculate weights for the attributes using Info Gain and Gain Ratio

# In the model we will exlude the following Columns:
# Id because it is the row ID
# CallStart and CallEnd because we have built the Column Duration which is the difference of those
# DaysPassed because based on this Column we created the IsContact which is a factor 
install.packages("FSelector", repos='http://cran.us.r-project.org')

weights_info_gain<-FSelector::information.gain(SALE ~ ., data=dataco)
weights_info_gain

weights_gain_ratio = FSelector::gain.ratio(SALE ~ ., data=dataco)
weights_gain_ratio

# Select the 12 most important attributes based on Gain Ratio
most_important_attributes <- FSelector::cutoff.k(weights_gain_ratio, 8)
most_important_attributes

formula_with_most_important_attributes <- FSelector::as.simple.formula(most_important_attributes, "SALE")
formula_with_most_important_attributes

# SALE ~ DRIVERAGE + LICENCEYEARSHELD + VEHICLEAGE + PREMIUM + 
#   PREVIOUSCLAIMS + QUOTEDATE + REGION + NOCLAIMSDISCOUNT


# Remove columns we didn't see influence on SALE from above
data <- data %>%
  dplyr::select(
    #-VEHICLEVALUE,
    -ID
  )
