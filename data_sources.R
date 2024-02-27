# Data sources and downloads
library(tidyverse)

# HADCRUT5
HADCRUT5 <- read_fwf(file = "https://climexp.knmi.nl/data/ihadcrut5_global.dat",
                     fwf_widths(c(6, rep(15, 12)), c("Year", month.abb)),
                     skip = 12,
                     na = "-999.9000",
                     show_col_types = FALSE) %>% 
  pivot_longer(cols = -Year, 
               names_to = "Month", 
               values_to = "Anomaly") %>% 
  mutate(Month = match(Month, month.abb)) %>% 
  mutate(Year_num = as.numeric(Year) + (Month - 1)/12)

write_csv(HADCRUT5, 
          file = "data/HADCRUT5.csv")

# CRUTEM5
CRUTEM5 <- read_fwf(file = "https://climexp.knmi.nl/data/icrutem5_global.dat",
                    fwf_widths(c(6, rep(15, 12)), c("Year", month.abb)),
                    skip = 13,
                    na = "-999.9000",
                    show_col_types = FALSE) %>% 
  pivot_longer(cols = -Year, 
               names_to = "Month", 
               values_to = "Anomaly") %>% 
  mutate(Month = match(Month, month.abb)) %>% 
  mutate(Year_num = as.numeric(Year) + (Month - 1)/12)
write_csv(CRUTEM5, 
          file = "data/CRUTEM5.csv")


# HADSST
# "https://climexp.knmi.nl/data/iHadSST4_monthly_GLOBE.dat"
HADSST <-  read_fwf(file = "https://climexp.knmi.nl/data/iHadSST4_monthly_GLOBE.dat", 
                   skip = 9,
                   fwf_widths(c(4, 3, 8), c("Year", "Month", "Anomaly")),
                   na = "-999.9000",
                   show_col_types = FALSE) |> 
  mutate(Anomaly = as.numeric(Anomaly)) |> 
  mutate(Year_num = as.numeric(Year) + (Month - 1)/12)
write_csv(HADSST, 
          file = "data/HADSST.csv")

  
  


# # GISTEMP
# # https://climexp.knmi.nl/data/igiss_al_gl_m.dat
# GISTEMP <-  read_fwf(file = "https://climexp.knmi.nl/data/igiss_al_gl_m.dat",
#                      fwf_widths(c(6, rep(15, 12)), c("Year", month.abb)),
#                      skip = 12,
#                      na = "-999.9000",
#                      show_col_types = FALSE) |> 
#   pivot_longer(cols = -Year, 
#                names_to = "Month", 
#                values_to = "Anomaly") |>  
#   mutate(Month = match(Month, month.abb)) |> 
#   mutate(Year_num = as.numeric(Year) + (Month - 1)/12)
# write_csv(GISTEMP, 
#           file = "data/GISTEMP.csv")

# NOAA/NCEI
# https://climexp.knmi.nl/data/incdc_gl.dat
NOAA_NCEI <- read_fwf(file = "https://climexp.knmi.nl/data/incdc_gl.dat", 
                      skip = 9,
                      fwf_widths(c(4, 3, 8), c("Year", "Month", "Anomaly")),
                      na = "-999.9000",
                      show_col_types = FALSE) |>  
  mutate(Month = as.numeric(Month)) |> 
  mutate(Year_num = as.numeric(Year) + (Month - 1)/12)
write_csv(NOAA_NCEI, 
          file = "data/NOAA_NCEI.csv")

# ERA 5
# https://climexp.knmi.nl/data/iera5_t2m_gl.dat
ERA_5 <-  read_fwf(file = "https://climexp.knmi.nl/data/iera5_t2m_gl.dat",
                     fwf_widths(c(6, rep(15, 12)), c("Year", month.abb)),
                     skip = 18,
                     na = "-999.9000",
                     show_col_types = FALSE) |>  
  pivot_longer(cols = -Year, 
               names_to = "Month", 
               values_to = "Anomaly") |>  
  mutate(Month = match(Month, month.abb)) |>  
  mutate(Year_num = as.numeric(Year) + (Month - 1)/12)
write_csv(ERA_5, 
          file = "data/ERA_5.csv")


# HAD CRUT4 Kriging
# https://climexp.knmi.nl/data/ihad4_krig_v2_0_0_gl.dat
HAD_CRUT4_Krig <- read_fwf(file = 'https://climexp.knmi.nl/data/ihad4_krig_v2_0_0_gl.dat', 
                              fwf_widths(c(8, 8), c("YearFraction", "Anomaly")), 
                              col_types = cols(YearFraction = col_double(), Anomaly = col_double()),
                              skip = 10)  %>%
  mutate(Year = floor(YearFraction)) |> 
  select(-YearFraction) |> 
  mutate(id = row_number()) |> 
  complete(id = full_seq(id, period = 1), fill = list(Anomaly = NA)) |> 
  mutate(Month = rep(1:12, length.out = n())) |> 
  select(-id)  |> 
  select(Year, Month, Anomaly) |> 
  mutate(Year_num = as.numeric(Year) + (Month - 1)/12)
write_csv(HAD_CRUT4_Krig, 
          file = "data/HAD_CRUT4_Krig.csv")

# Berkeley
# https://climexp.knmi.nl/data/it2m_land_ocean_best.dat
Berkeley <- read_fwf(file = "https://climexp.knmi.nl/data/it2m_land_ocean_best.dat", 
                      skip = 8,
                      fwf_widths(c(6, 6, 10), c("Year", "Month", "Anomaly")),
                      na = "-999.9000",
                      show_col_types = FALSE) |>  
  mutate(Month = as.numeric(Month)) |> 
  mutate(Year_num = as.numeric(Year) + (Month - 1)/12) |> 
  slice(1:(n() - 2))
write_csv(Berkeley, 
          file = "data/Berkeley.csv")




# NASA
# https://climexp.knmi.nl/data/igiss_al_gl_m.dat
NASA_GISS <-  read_fwf(file = "https://climexp.knmi.nl/data/igiss_al_gl_m.dat",
                  fwf_widths(c(6, rep(15, 12)), c("Year", month.abb)),
                  skip = 12,
                  na = "-999.9000",
                  show_col_types = FALSE) %>% 
  pivot_longer(cols = -Year, 
               names_to = "Month", 
               values_to = "Anomaly") %>% 
  mutate(Month = match(Month, month.abb)) %>% 
  mutate(Year_num = as.numeric(Year) + (Month - 1)/12)
write_csv(NASA_GISS, 
          file = "data/NASA_GISS.csv")





