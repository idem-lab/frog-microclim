# this script reproduces the subroutine to get data from NicheMapR package

# optionally install package if you haven't already - note Fortran-related dependencies
# devtools::install_github('mrke/NicheMapR')

library(lattice)
library(patchwork)
library(tidyverse)
library(NicheMapR)
library(lubridate)
# get the raw data 
get.global.climate(folder="data/")

# as an example extract micro climate for a point location near western treatment plant in Melbourne - the coordinate is from the real life location of a growling grass frog sighting on Atlas of Living Australia https://biocache.ala.org.au/occurrences/120f9433-5f3b-4a47-bf32-31643e9b53fa
loc <- c(144.5963,-37.9714)

# in all following simulations I have chosen arbitrary values for shading and for reflectiveness of rock surfaces, these values should be compared against experimental estimates where available

# build micro climate projections for partially shaded habitats
micro <- micro_global(loc = loc,
                      #timeinterval = 365, # optionally adjust frequency of day-of-year to calculate from
                      minshade = 50, # input parameter for the light level of shading 
                      maxshade = 90 # input parameter for the dense level of shading 
                      )
# build micro climate projections for sun-heated rock surface
micro_rocks <- micro_global(loc = loc,
                      #timeinterval = 365,
                      runshade = 0, # switching off shading model
                      minshade = 0, # switching off shading model
                      soiltype = 0, # sets soil type to rock
                      REFL = 1e-6 # solar radiation reflection, this value makes a dark rock that abosrbs a lot of heat from the sun
)

# wrangling of prediction results for plotting
light_veg <- as.data.frame(micro$soil) %>% 
  select("DOY","TIME","D0cm") %>% 
  mutate(habitat = "light shading")
dense_veg <- as.data.frame(micro$shadsoil) %>% 
  select("DOY","TIME","D0cm")%>% 
  mutate(habitat = "dense shading")
heated <- as.data.frame(micro_rocks$soil) %>% 
  select("DOY","TIME","D0cm")%>% 
  mutate(habitat = "rock in sun")

habitats <- bind_rows(light_veg,dense_veg,heated) %>% 
  mutate(month = month(as_date(DOY, origin = "2023-01-01"), 
                       label = TRUE, 
                       abbr = TRUE))


temp_plot <- ggplot(habitats,aes(x =TIME, y = D0cm, colour = habitat)) + 
  geom_line() +
  facet_wrap(~month,) + 
  scale_x_continuous(name = "time of day in minutes") + 
  scale_y_continuous(name = "ground surface temperature in degrees Celsius") + 
  theme_bw() + 
  ggtitle("Micro habitat temperature in western Melbourne")
ggsave("figs/demo-timeseries-temp.png",temp_plot)


# test out air humidity predictions
light_veg_humidity <- as.data.frame(micro$metout) %>% 
  select("DOY","TIME","RHLOC") %>% 
  mutate(habitat = "light shading")
dense_veg_humidity <- as.data.frame(micro$shadmet) %>% 
  select("DOY","TIME","RHLOC")%>% 
  mutate(habitat = "dense shading")
heated_humidity <- as.data.frame(micro_rocks$metout) %>% 
  select("DOY","TIME","RHLOC")%>% 
  mutate(habitat = "rock in sun")

habitats_humidity <- bind_rows(light_veg_humidity,
                               dense_veg_humidity,
                               heated_humidity) %>% 
  mutate(month = month(as_date(DOY, origin = "2023-01-01"), 
                       label = TRUE, 
                       abbr = TRUE))


humidity_plot <- ggplot(habitats_humidity,aes(x =TIME, y = RHLOC, colour = habitat)) + 
  geom_line() +
  facet_wrap(~month,) + 
  scale_x_continuous(name = "time of day in minutes") + 
  scale_y_continuous(name = "relative air humidity (%) at 1cm above ground") + 
  theme_bw() + 
  ggtitle("Micro habitat humidity in western Melbourne")

wrap_plots(temp_plot,
           humidity_plot,nrow = 2, ncol = 1,guides = "collect")

ggsave("figs/demo-timeseries_both.png",width = 8, height = 8, unit = "in")
