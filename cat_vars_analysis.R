#CATEGORICAL STATS

# Decrease graph size from standard
options(repr.plot.width = 3, repr.plot.height = 3)
dev.off()
# Function to generate graphs for factor variables and churn

## Extract columns to be analyzed
function_columns <- data %>%
  dplyr::select(
    'REGION', 
    #'VEHICLETYPE', 
    'INCEPTIONDATECAT', 
   # 'QUOTEINT',
   # 'CATAGE',
   'DRIVERAGE',
  # 'INCEPTIONDATE',
    'SALE'
  )

## Function, goes through each column selected
for (i in 1:ncol(function_columns))
{
  # Get column names so dplyr group by works
  cname <- colnames(function_columns[c(i,4)])
  # Subset data frame by variable name selected
  a <- subset(
    function_columns, !is.na(function_columns[,i]) & function_columns[,i] != "",
    select = cname
  ) %>%
    # Create percentage statistics per variable
    group_by_at(vars(cname)) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 2)
    )
  
  # Save plot in a variable so plots can be displayed sequentialy
  p <- ggplot(
    data = a, aes_string(
      x = colnames(a[1]), y = colnames(a[4]), fill = colnames(a[1])
    )
  ) +
    # Split each graph per Churn to see influence of variable
    facet_wrap("SALE") + 
    geom_bar(stat = "identity") +
    # Make graph a bit cleaner
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = 70, hjust = 1),
      legend.position="none"
    ) +
    geom_text(
      aes(y = Percentage, label = paste0(Percentage * 100,"%"))
    ) +
    labs(
      x = colnames(a[1]), y = "SALE", title = paste("SALE and", colnames(a[1]))
    )
  
  # Display graphs
  print(p)
  # Cleanup
  rm(cname, a, p)
}


#We can see, VEHICLE TYPE, INCEPTIONDATECAT
#do not seem to have an influence on SALE.
#so these will be removed