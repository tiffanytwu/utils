 ## UDF helper functions for plotting
 
library(ggplot2)
library(dplyr)
 
##Stacked Faceted Boxplots By Group
boxplot_facets <- function(df, group, metric, facetvar){
   df %>% 
     ggplot(aes_string(x = group, y=metric, color=group)) + 
     geom_boxplot() + 
     facet_grid(as.formula(paste(facetvar, "~ .")), switch = "y") + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1), 
           strip.text.y = element_text(angle = 180)) + 
    xlab(facetvar) +
     coord_flip() 
}

binaryBarChart <- function(df, metric, group, facetvar){
 df %>%
   group_by_(facetvar, group) %>% 
   summarise_( pct = paste0('mean(', metric , ', na.rm=T)')) %>% 
   ggplot(aes_string(x=group, y='pct', fill=group)) +
   geom_bar(stat='identity') +
   facet_grid(as.formula(paste(facetvar, "~ .")), switch = "y") + 
   theme(axis.text.x = element_text(angle=90, hjust=1), strip.text.y = element_text(angle = 180)) +
   scale_y_continuous(limits = c(0,1)) +
   xlab(facetvar) +
   coord_flip()
}
 
#To create pdf for all numerical variables

#grab all numerical variables from data frame
numerical_feats <- colnames(df)[sapply(BQ, is.numeric)]

pdf("FacetBoxPlots.pdf")
 for(feat in numerical_feats){
   print(boxplot_facets(data, 'category', numerical, 'eligible'))
}
dev.off()
