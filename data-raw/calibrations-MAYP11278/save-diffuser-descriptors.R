# D7-H-SMA from 2011

D7_cosine.ls <- list(make = "Bentham Instruments",
                     model = "D7-H-SMA",
                     geometry = "cosine",
                     serial.number = NA_character_,
                     area = pi * (10e-3 / 2)^2) # 10 mm diameter disk

saveRDS(D7_cosine.ls, file = "./data-raw/calibrations-MAYP11278/d7-h-2011.rds")

D7_dome.ls <- list(make = "Bentham Instruments",
                   model = "D7-custom-SMA",
                   geometry = "hemispherical",
                   serial.number = NA_character_,
                   area = 2 * pi * (10e-3 / 2)^2) # 10 mm diameter hemisphere

saveRDS(D7_dome.ls, file = "./data-raw/calibrations-MAYP11278/d7-d-2021.rds")
