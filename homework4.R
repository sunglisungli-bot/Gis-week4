install.packages(c(
  "readr","readxl","dplyr","tidyr","janitor","stringr",
  "sf","countrycode","ggplot2","rnaturalearth","rnaturalearthdata"
))

library(readr); library(readxl)
library(dplyr); library(tidyr); library(janitor); library(stringr)
library(sf); library(countrycode)
library(ggplot2)
library(rnaturalearth); library(rnaturalearthdata)

world_shp <- "D:/CASA05/project 4/Gis-week4/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp"

world_sf <- st_read(world_shp) %>%
  clean_names()

# 数据里有 country / iso / countryaff / aff_iso
# 统一成 iso3c 以便和指标联接
world_sf <- world_sf %>%
  dplyr::rename(iso3c = iso) %>%
  mutate(iso3c = as.character(iso3c))

gii_csv <- "D:/CASA05/project 4/Gis-week4/HDR25_Composite_indices_complete_time_series.csv"

ts_raw <- read_csv(gii_csv) %>%
  clean_names()

names(ts_raw)[1:30]  # 看看前30列名（调试用）

names(ts_raw)[1:40]

names(ts_raw)[str_detect(names(ts_raw), "gii")]

#  只保留国家、ISO3 和 GII 列
gii_data <- ts_raw %>%
  select(iso3, country, starts_with("gii_")) %>%
  clean_names()

gii_change <- gii_data %>%
  select(iso3, country, gii_2010, gii_2019) %>%
  mutate(
    gii_2010 = as.numeric(gii_2010),
    gii_2019 = as.numeric(gii_2019),
    gii_diff = gii_2019 - gii_2010
  )

world_gii <- world_sf %>%
  left_join(gii_change, by = c("iso3c" = "iso3"))

# 地图里的国家代码
head(world_sf$iso3c)

# 数据里的国家代码
head(gii_change$iso3)

# 看看是否有重叠
intersect(world_sf$iso3c, gii_change$iso3)[1:10]

sum(world_sf$iso3c %in% gii_change$iso3)
length(world_sf$iso3c)

unique(gii_change$iso3)[1:20]

library(countrycode)

world_sf <- world_sf %>%
  mutate(
    iso3 = countrycode(iso3c, origin = "iso2c", destination = "iso3c")
  )

head(world_sf[, c("iso3c", "iso3")])

world_gii <- world_sf %>%
  left_join(gii_change, by = "iso3")

ggplot(world_gii) +
  geom_sf(aes(fill = gii_diff), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  
  theme_minimal()

dir.create("outputs", showWarnings = FALSE)

# 仅属性表
world_gii %>%
  st_drop_geometry() %>%
  write.csv("outputs/world_gii_2010_2019.csv", row.names = FALSE)

# 空间数据）
st_write(world_gii, "outputs/world_gii_2010_2019.geojson", delete_dsn = TRUE)





