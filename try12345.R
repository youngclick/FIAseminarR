library(readr)
library(dplyr)
library(sf)
library(ggplot2)
tree_raw_data_with_env_cleaned <- read_csv("G:/Rtestproject/data/tree_raw_data_with_env_cleaned.csv")

View(tree_raw_data_with_env_cleaned)

dim(tree_raw_data_with_env_cleaned)

head(tree_raw_data_with_env_cleaned)

str(tree_raw_data_with_env_cleaned)

##adding common name to the data frame
REF_SPECIES <- read_csv("G:/Rtestproject/data/REF_SPECIES.csv")
head(REF_SPECIES)

commonname<-REF_SPECIES%>%
  select(SPCD,COMMON_NAME,GENUS,SPECIES)

tree_raw_common<-left_join(tree_raw_data_with_env_cleaned,commonname, by=c("spcd"="SPCD"),relationship = "many-to-many")
head(tree_raw_common)


##rows with na
rows_with_na <- tree_raw_common[apply(is.na(tree_raw_common), 1, any), ]

unique(rows_with_na$statename) ##states with na entries

unique(rows_with_na$COMMON_NAME) ##species with na entries

View(rows_with_na)

nrow(rows_with_na) ###683617 rows

##remove na   ##683617 rows removed
nona_tree_raw<-na.omit(tree_raw_common)
nrow(nona_tree_raw)
View(nona_tree_raw)

##summary
summary(nona_tree_raw)
View(nona_tree_raw)

##ggplot

#Eastern US Tree Heights Distribution
plot_tree_height<-ggplot(nona_tree_raw, aes(x = ht)) +
  geom_histogram(binwidth = 5,color = "black") +
  labs(title = "Eastern US Tree Heights Distribution")
plot_tree_height

#Eastern US Tree Diameter Distribution

plot_tree_dia<-ggplot(nona_tree_raw,aes(x=dia))+
  geom_histogram(binwidth = 0.2,color = "blue")+
  labs(title = "Eastern US Tree Diameter Distribution")
plot_tree_dia

#Eastern US Scatter Plot of Tree Height Diameter
nona_tree_raw$common
scatter_plot_ht_dia<-ggplot(nona_tree_raw, aes(x = ht, y = dia)) +
  geom_point(color="red") +
  labs(title = "Eastern US Scatter Plot of Tree Height Diameter")
scatter_plot_ht_dia

scatter_plot_ht_dia_facetwrap <- ggplot(nona_tree_raw, aes(x = ht, y = dia, color = common)) +
  geom_point() +
  facet_wrap(~common) +
  labs(title = "Eastern US Scatter Plot of Tree Height vs. Diameter",
       x = "Tree Height",
       y = "Diameter")
scatter_plot_ht_dia_facetwrap


