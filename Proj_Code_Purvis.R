fia_grid_df <- as.data.frame(fia_grid)

### EDA

library(ggplot2)

ncol(fia_df)
nrow(fia_df)
dim(fia_df)
str(fia_df)
names(fia_df)
fia_df


##looking at the top 30 species in terms of absolute trees
top_30_species <- fia_df %>%
  group_by(COMMON_NAME) %>%
  summarise(absolute_trees = sum(X)) %>%
  arrange(-absolute_trees) %>%
  slice(1:30)

top_30_species
ggplot(top_30_species, aes(x = absolute_trees, y = COMMON_NAME, label = absolute_trees))+
  geom_bar(stat = "identity")+
  geom_text(size = 4)+
  labs(title = "Top 30 Species in Terms of Absolute Trees", 
       x = "Number of Trees", 
       y = "Common Name")

##number of individual species
number_of_species <- length(unique(fia_df$COMMON_NAME))
number_of_species


##plotting of dia and ht
View(fia_df)
ht_count_df <- ggplot(fia_df, aes(x = ht))+
  geom_histogram(binwidth = 1, color = "black", fill = "white")+
  labs(title = "Height of Trees", 
       x = "Height")
ht_count_df

ht_count_density <- ggplot(fia_df, aes(x = ht))+
  geom_histogram(aes(y =..density..), color = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666")+
  labs(title = "Density of Height of Trees",
       x = "Height")
ht_count_density

dia_count_df <- ggplot(fia_df, aes(x = dia))+
  geom_histogram(binwidth = 1, color = "black", fill ="white")+
  labs(title = "Diameter of Trees",
       x = "Diameter")
dia_count_df

dia_count_density <- ggplot(fia_df, aes(x = dia))+
  geom_histogram(binwidth = 2, aes(y = ..density..), color = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666")+
  labs(title = "Density of Diameter of Trees",
       x = "Diameter")
dia_count_density

tree_ht = ggplot(fia_df, aes(x = ht)) +
  geom_bar()+
  labs(title = "Height of Trees",
       x = "Height")
tree_ht

tree_dia = ggplot(fia_df, aes(x = dia))+
  geom_bar()+
  labs(title = "Diameter of Trees",
       x = "Diameter")
tree_dia



# Display the structure of the data
str(fia_df)

# Display summary statistics
summary(fia_df)

# Display the first few rows of the data
head(fia_df)

# Check for missing values
print(colSums(is.na(fia_df)))

# Visualize the distribution of numeric variables
numeric_vars <- select_if(fia_df, is.numeric)
numeric_vars


print(fia_df)
#9 variables
#8 are numerical
#1 is character
summary(fia_df)

#NAs for dia 1425
#NAs for ht 5380

#graphing
library(raster)
library(sp)
library(sf)
library(maps)
library(grid)
library(tmap)


#### calc species richness at grid level

grid_sr <- fia_df %>%
  group_by(id) %>%
  summarise(SR = n_distinct(spcd))

n_distinct(grid_sr$id)

fia_grid_df <- merge(fia_df, grid_sr, by.x = "id", by.y = "id")
fia_grid_df

###calc avg sr aggregated at the latitudinal band
###calc moving avg at 0.5 degree interval


myseq <- seq(from = 25, to = 50, by = 0.5)
for (i in myseq){
  print(i+1)
}

lat_min <- myseq[2] - 0.5
lat_min
lat_max <- myseq[2] + 0.5
lat_max
length(myseq)
length(myseq)

sr_bands <- data.frame(matrix(ncol = 2, nrow = length(myseq)))
names(sr_bands) <- c("LatBands","SR")
sr_bands

for (i in 1:length(myseq)){
  lat_min <- myseq[i] - 0.5
  lat_max <- myseq[i] + 0.5
  fia_df_subset <- fia_df %>%
    filter(centroid_lat > lat_min & centroid_lat <= lat_max)
  sr <- length(unique(fia_df_subset$COMMON_NAME))
  sr_bands[i,1] <- myseq[i]
  sr_bands[i,2] <- sr
}

sr_bands
ggplot(data = sr_bands, aes(x = LatBands, y = SR)) +
  geom_point()+
  labs(title = "Species Richness by Latitude Bands")

###Estimate the SR using bootstrapping methods (1000 times simulation) and report mean and std. sr

LatRichRandom <- data.frame(
  Latitude = 0,
  Richness = 0
)

for (i in 1:25){
  current_range <- fia_grid_df %>%
    filter((i+24) < centroid_lat & centroid_lat < (i+25))
  LatRichSample <- data.frame(
    Run = 0,
    Richness = 0
  )
  for (j in 1:1000){
    current_frac <- current_range %>% slice_sample(n = 100)
    LatRichSample[j,1] = j
    LatRichSample[j,2] = n_distinct(current_frac$SPECIES)
  }
  LatRichRandom[i,1] = i+24
  LatRichRandom[i,2] = mean(LatRichSample$Richness)
}


LatRichRandom2 <- LatRichRandom
LatRichRandom2


# Set seed for reproducibility
set.seed(123)

# Number of bootstrap samples
n_bootstrap <- 1000

# Create a data frame to store bootstrap results
results <- data.frame(id = unique(fia_grid_df$id))

# Perform bootstrapping using a for loop
for (i in 1:n_bootstrap) {
  sampled_df <- fia_grid_df[sample(1:nrow(fia_grid_df), replace = TRUE), ]
  grid_sr <- sampled_df %>%
    group_by(id) %>%
    summarise(SR = n_distinct(spcd))
  
  # Merge the bootstrap results with the original data
  results <- merge(results, grid_sr, by = "id", all.x = TRUE)
  colnames(results)[colnames(results) == "SR"] <- paste0("bootstrap_", i)
}

# Calculate mean and standard deviation of SR across bootstrap samples
results$mean_SR <- rowMeans(results[, -(1:1)], na.rm = TRUE)
results$std_SR <- apply(results[, -(1:1)], 1, sd, na.rm = TRUE)


# Print the mean and standard deviation
cat("Mean SR:", mean(results$mean_SR, na.rm = TRUE), "\n")
cat("Standard Deviation of SR:", sd(results$std_SR, na.rm = TRUE), "\n")

# Merge centroid latitude and longitude into the results data frame
results <- merge(results, fia_grid_df[, c("id", "centroid_lat", "centroid_Long")], by = "id", all.x = TRUE)
results

```
### Plotting
