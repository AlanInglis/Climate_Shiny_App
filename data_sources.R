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

# GISTEMP
# https://climexp.knmi.nl/data/igiss_al_gl_m.dat

# NOAA/NCEI
# https://climexp.knmi.nl/data/incdc_gl.dat

# ERA 5
# https://climexp.knmi.nl/data/iera5_t2m_gl.dat

# HAD CRUT4 Kriging
# https://climexp.knmi.nl/data/ihad4_krig_v2_0_0_gl.dat

# Berkeley
# https://climexp.knmi.nl/data/it2m_land_ocean_best.dat