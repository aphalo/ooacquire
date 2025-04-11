# D7-H-SMA from 2019

D7_cosine.ls <- list(make = "Bentham Instruments",
                     model = "D7-H-SMA",
                     geometry = "cosine",
                     serial.number = NA_character_,
                     area = pi * (10e-3 / 2)^2, # 10 mm diameter disk
                     fibre = NA_character_)

saveRDS(D7_cosine.ls, file = "./data-raw/calibrations-MAYP114590/d7-h-2019.rds")
