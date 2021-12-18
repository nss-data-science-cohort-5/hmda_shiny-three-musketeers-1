library(tidyverse)
library(sf)

hmda_lei_census <- read_csv("data/hmda_lei_census.csv")
wash <- read_sf("data/wash.shp")

# Take a filtered dataset that *cannot* be modified by geography.
map_data <- hmda_lei_census %>% 
  count(Name) %>% 
  rename(Aggregate_Number = n) %>% 
  mutate(Name = str_remove(Name, " County, Washington")) %>% 
  right_join(wash, by = c("Name" = "NAME")) %>% 
  st_as_sf()

# Pull out centroids as shapes in separate column using sf.
map_data$centroids <- map_data %>% 
  st_centroid() %>% 
  st_geometry()

# Pull out separate coordinates as columns from dataset.
map_data <- map_data %>% 
  mutate(lat = unlist(map(map_data$centroids,1)),
         long = unlist(map(map_data$centroids,2)))

# create map.
map_data %>% 
  ggplot() + 
  geom_sf(aes(fill = Aggregate_Number),
          lwd = 0) +
  geom_text(aes(x = lat,
                y = long,
                label = scales::comma(Aggregate_Number)),
            size = 4) + 
  labs(title = "Applicant Number by County") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 20),
        axis.line=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        legend.position = "None") + 
  scale_fill_gradient(low = "#10bee8", 
                      high = "#A0522D",
                      trans = "log2")

