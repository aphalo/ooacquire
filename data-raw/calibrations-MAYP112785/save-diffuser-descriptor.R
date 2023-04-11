# D7-H-SMA from around 2016

D7_cosine.ls <- list(make = "Bentham Instruments",
                     model = "D7-H-SMA",
                     geometry = "cosine",
                     serial.number = "20595/1",
                     area = pi * (10e-3 / 2)^2, # 10 mm diameter disk
                     fibre = "QP400-2-SR-BX, 00S-005717-30")

saveRDS(D7_cosine.ls, file = "./data-raw/calibrations-MAYP112785/d7-h-2019.rds")

# I need to check old calibration certificates to know what fibre was used before the 2019 calibration.
D7_cosine_old.ls <- list(make = "Bentham Instruments",
                     model = "D7-H-SMA",
                     geometry = "cosine",
                     serial.number = "20595/1",
                     area = pi * (10e-3 / 2)^2, # 10 mm diameter disk
                     fibre = "unknown")

saveRDS(D7_cosine_old.ls, file = "./data-raw/calibrations-MAYP112785/d7-h-pre-2019.rds")

