library(tidyverse)
library(sf)
library(here)
library(janitor)
library(countrycode)
library(tidyverse)
library(dplyr)
library(readr)
library(tmap)
#read shp
World <- st_read('/Users/alexia/Documents/gis_code/World_Countries_(Generalized)/World_Countries_Generalized.shp')
install.packages("usethis")
library(usethis)
#read csv
HDI <- read_csv(here::here("Documents/gis_code/HDR25_Composite_indices_complete_time_series.csv"),
                locale = locale(encoding = "latin1"),
                na = " ", skip=0)
problems(HDI)
HDI <- read_csv(
  here::here("Documents/gis_code/HDR25_Composite_indices_complete_time_series.csv"),
  locale = locale(encoding = "latin1"),
  na = c("", " ", "NA", "..", "n/a")
)
problems(HDI)
HDIcols<- HDI %>%
  clean_names()%>%
  select(iso3, country, gii_2019, gii_2010)%>%
  mutate(difference=gii_2019-gii_2010)%>%
  #not needed here as we can now use the country name...but see below
  mutate(iso_code=countrycode(country, origin = 'country.name', destination = 'iso2c'))%>%
  mutate(iso_code2=countrycode(iso3, origin ='iso3c', destination = 'iso2c'))

Join_HDI <- World %>% 
  clean_names() %>%
  left_join(., 
            HDIcols,
            # change to "aff_iso" = "iso_code"
            by = c("iso" = "iso_code"))

# 261 if using "aff_iso", 251 if using "iso". Could filter out the NA values.

Join_HDI_2 <- World %>% 
  clean_names() %>%
  left_join(., 
            HDIcols,
            by = c("country" = "country"))
#251

# difference only if used aff_iso to join and make Join_HDI
Join_HDI_GB<-Join_HDI %>%
  filter(aff_iso=="GB")

Join_HDI_2_GB<-Join_HDI_2 %>%
  filter(aff_iso=="GB")

World_clean <- World %>% clean_names()
names(World_clean)
key_col <- "iso_a2"

HDIcols <- HDI %>%
  clean_names() %>%
  select(iso3, country, gii_2019, gii_2010) %>%
  mutate(difference = gii_2019 - gii_2010,
         iso_code   = countrycode(country, origin = "country.name", destination = "iso2c")) %>%
  select(country, iso3, iso_code, gii_2010, gii_2019, difference)

World_clean <- World %>% clean_names() %>% st_transform(4326)

Join_HDI <- World_clean %>%
  left_join(HDIcols, by = setNames("iso_code", key_col))
World_clean <- World %>% clean_names() %>% st_transform(4326)
names(World_clean)

HDIcols <- HDI %>%
  clean_names() %>%
  select(iso3, country, gii_2019, gii_2010) %>%
  mutate(difference = gii_2019 - gii_2010,
         iso_code   = countrycode(country, "country.name", "iso2c")) %>%
  select(country, iso3, iso_code, gii_2010, gii_2019, difference)

key_col <- "aff_iso"
Join_HDI <- World_clean %>%
  left_join(HDIcols, by = setNames("iso_code", key_col))

HDIcols <- HDI %>%
  clean_names() %>%
  select(iso3, country, gii_2019, gii_2010) %>%
  mutate(difference = gii_2019 - gii_2010)

key_col <- "iso3" 
Join_HDI <- World_clean %>%
  left_join(HDIcols, by = setNames("iso3", key_col))

key_col <- "country"
Join_HDI <- World_clean %>%
  left_join(HDIcols %>% clean_names(), by = setNames("country", key_col))

library(tmap)
tmap_mode("plot")

tm1 <- tm_shape(Join_HDI %>% filter(!is.na(difference))) +
  tm_polygons(
    col = "difference",
    palette = "-RdYlGn",
    style = "quantile",
    title = "GII change (2010→2019)"
  ) +
  tm_borders(col = "gray40", lwd = 0.2) +
  tm_layout(legend.outside = TRUE, frame = FALSE) +
  tm_scale_bar() + tm_compass(type = "8star")

gb_key <- key_col
gb_val <- if (gb_key %in% c("aff_iso","iso_a2","iso2","iso")) "GB" else "GBR"

tm_gb <- tm_shape(Join_HDI %>% filter(.data[[gb_key]] == gb_val)) +
  tm_borders(col = "black", lwd = 2)

tm1 + tm_gb

library(ggplot2)

World_clean <- World %>% clean_names() %>% st_transform(4326)

cand_name_cols <- c("country","name","name_long","name_en","admin")
name_col <- cand_name_cols[cand_name_cols %in% names(World_clean)][1]
if (is.na(name_col)) stop("No country name column found in world shapefile.")

World_iso <- World_clean %>%
  mutate(iso3_join = countrycode(.data[[name_col]], origin = "country.name", destination = "iso3c"))

Join_HDI <- World_iso %>%
  left_join(HDIcols, by = c("iso3_join" = "iso3"))

p <- ggplot(Join_HDI) +
  geom_sf(aes(fill = difference), color = "grey40", linewidth = 0.2, na.rm = TRUE) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1, na.value = "grey90", name = "GII change (2010–2019)") +
  labs(title = "Change in Gender Inequality Index (2010–2019)") +
  theme_minimal() +
  theme(legend.position = "right")

gb <- Join_HDI %>% filter(iso3_join == "GBR")
if (nrow(gb) > 0) {
  p <- p + geom_sf(data = gb, fill = NA, color = "black", linewidth = 0.9)
}
print(p)

