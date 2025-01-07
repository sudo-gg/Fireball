library(tidyverse)
library(ggmap)
df <- read.csv("cneos_fireball_data.csv",stringsAsFactors = FALSE)
meanVelocity <- mean(df$Velocity..km.s., na.rm=T)
colnames(df) <- c("Date", "Latitude (deg)", "Longitude (deg)", "Altitude (km)", "Velocity (km/s)","vx","vy","vz","Total Radiated Energy (J)","Total Impact Energy (kt)")

convertCoords <- function(coord){
  if(coord == "" || is.na(coord)) {
    return(NA)  # Return NA for missing or empty coordinates
  }
  # regex for only keeping numbers 0-9 at the start (remove NSEW) and one for storing NSEW to determine if negative
  numeric_part <- as.numeric(gsub("[^0-9\\.]","",coord))
  direction <- gsub("[^NSEW]","",coord)
  
  if (direction %in% c("S","W")){
    return(-numeric_part)
  }else{
    return(numeric_part)
  }
}

calculateFlux <- function(coord){
  fluxList = c()
}

df$`Latitude (deg)` <- sapply(df$`Latitude (deg)`, convertCoords)
df$`Longitude (deg)` <- sapply(df$`Longitude (deg)`, convertCoords)

df <- df %>% separate(Date,into=c("Date","Time"),sep=" ")

df$Date <- as.Date(df$Date)



# Function to calculate bands for a certain direction
calculate_band_lat <- function(df,band_size = 10) {
  df$band <- cut(df$`Latitude (deg)`,breaks = seq(-90,90,by = band_size),
                 include.lowest = T)
  band_count <- df %>% 
                group_by(band) %>%
                summarise(count = n())
  return(band_count)
}

calculate_band_long <- function(df,band_size = 20) {
  df$band <- cut(df$`Longitude (deg)`,breaks = seq(-180,180,by = band_size),
                 include.lowest = T)
  band_count <- df %>% 
    group_by(band) %>%
    summarise(count = n())
  return(band_count)
}

View(df)

# Function to categorize lat/lon into bands
calculate_bands <- function(df, lat_band_size = 20, lon_band_size = 20) {
  # Cut latitude into bands
  df$lat_band <- cut(
    df$`Latitude (deg)`, 
    breaks = seq(-90, 90, by = lat_band_size), 
    include.lowest = T
  )
  
  # Cut longitude into bands
  df$lon_band <- cut(
    df$`Longitude (deg)`, 
    breaks = seq(-180, 180, by = lon_band_size), 
    include.lowest = T
  )
  
  # Count the number of meteorites in each lat/lon band
  band_counts <- df %>%
    group_by(lat_band, lon_band) %>%
    summarise(count = n())
    #arrange(desc(count))
  
  return(band_counts)
}
write.csv(calculate_bands(df),"asbdsf.csv")
# Applying the function to calculate the bands with 20-degree band size
#band_counts <- calculate_bands(df, lat_band_size = 20, lon_band_size = 20)
#print(band_counts)
write.csv(calculate_bands(df),"Grid_Totals.csv")
write.csv(calculate_band_lat(df),"Latitude_band_Totals.csv")
write.csv(calculate_band_long(df),"Longitude_band_totals.csv")
print(lat_count)


# Define the location and zoom level for the map (adjust these to your region of interest)
center_lon <- mean(df$`Longitude (deg)`)
center_lat <- mean(df$`Latitude (deg)`)
zoom_level <- 4  # Adjust zoom level depending on how zoomed-in you want the map

# Get the static map from Stamen Maps (you can also try 'google' or other maptypes)
base_map <- get_map(location = c(lon = center_lon, lat = center_lat), zoom = zoom_level, source = "google", maptype = "terrain")
# Does a 2D kernel density estimation
heatmap <- ggplot(df, aes(x = `Longitude (deg)`, y = `Latitude (deg)`)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon", alpha = 0.7) +
  scale_fill_gradient(low = "yellow", high = "red") +  # Gradient for density
  labs(title = "Meteorite Impact Flux (Density Heatmap)", x = "Longitude (deg)", y = "Latitude (deg)") +
  theme_minimal()
heatmap
heatmap2 <- ggplot(df, aes(x = `Longitude (deg)`, y = `Latitude (deg)`)) +
  geom_bin2d(bins = 30) +  # Adjust 'bins' to control the number of squares
  scale_fill_gradient(low = "lightblue", high = "red") +  # Color gradient for count of impacts
  labs(title = "Meteorite Impact Locations", x = "Longitude (deg)", y = "Latitude (deg)") +
  theme_minimal()
heatmap2
altvsVeloc <- df_clean %>%
  ggplot(aes(x = `Altitude (km)`, y = `Velocity (km/s)`)) +
  geom_point(color = "blue", alpha = 0.7) +  # Scatter points
  labs(title = "Altitude vs. Velocity of Meteorites", x = "Altitude (km)", y = "Velocity (km/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  geom_smooth(color="darkgreen",method = "loess")
altvsVeloc

lm_model <- lm(`Velocity (km/s)` ~ `Altitude (km)`, data = df_clean)
library(randomForest)
smooth_model <- randomForest(`Velocity (km/s)` ~ `Altitude (km)`, data = df_clean)
summary(lm_model)
cor(df$`Altitude (km)`, df$`Velocity (km/s)`, method = "pearson", use = "complete.obs")
cor(df$`Altitude (km)`, df$`Velocity (km/s)`, method = "spearman", use = "complete.obs")
cor(df$`Altitude (km)`, df$`Velocity (km/s)`, method = "kendall", use = "complete.obs")


df_clean <- df %>% 
  drop_na(`Velocity (km/s)`,`Altitude (km)`) %>%
  select(`Velocity (km/s)`,`Altitude (km)`) %>%
  filter(!is.na(`Velocity (km/s)`), !is.na(`Altitude (km)`)) %>%
  view()
write.csv(df_clean,"VelAlt.csv")

# Load necessary library
library(MASS)

# Remove rows with missing or infinite values in Longitude or Latitude
df_clean2 <- df[!is.na(df$`Longitude (deg)`) | !is.na(df$`Latitude (deg)`)]
tail(df_clean2)
kde <- kde2d(df_clean2$`Longitude (deg)`, df_clean2$`Latitude (deg)`, n = 100)

# Convert the density estimate to a data frame
density_df <- data.frame(Longitude = kde$x, Latitude = kde$y, Density = as.vector(kde$z))
density_df <- density_df %>%
  arrange(desc(Density))
density_df
# Find the coordinates with the highest density
max_density_point <- density_df[density_df$Density == max(density_df$Density), ]
print(max_density_point)


#averages <- df %>% summarise("Average radiated Energy" = mean(`Total Radiated Energy (J)`),
#                             "Average impact Energy" = mean(`Total Impact Energy (J)`))

#missing_values <- colSums(is.na(df))

#print(missing_values)
#print(nrow(df)-191)

#summary(df)
#convertCoords(df)
#glimpse(df)
#convertCoords(df)
#nrow(df)
#write.csv(df,"fireballdata1.csv")
