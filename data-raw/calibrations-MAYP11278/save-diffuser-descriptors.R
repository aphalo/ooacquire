# D7-H-SMA from 2011

D7_cosine.ls <- list(make = "Bentham Instruments",
                     model = "D7-H-SMA",
                     geometry = "cosine",
                     serial.number = "12321",
                     area = pi * (10e-3 / 2)^2, # 10 mm diameter disk
                     fibre = "QP400-2-SR-BX 00S-004728-16")

saveRDS(D7_cosine.ls, file = "./data-raw/calibrations-MAYP11278/d7-h-2023.rds")

# fibre serial number and length need checking in calibration certificates from before 2019.
D7_cosine.ls <- list(make = "Bentham Instruments",
                     model = "D7-H-SMA",
                     geometry = "cosine",
                     serial.number = "12321",
                     area = pi * (10e-3 / 2)^2, # 10 mm diameter disk
                     fibre = "unknown")

saveRDS(D7_cosine.ls, file = "./data-raw/calibrations-MAYP11278/d7-h-pre-2019.rds")

D7_dome.ls <- list(make = "Bentham Instruments",
                   model = "D7-SMA-0001 (custom)",
                   geometry = "hemispherical",
                   serial.number = "35702",
                   area = 2 * pi * (10e-3 / 2)^2, # 10 mm diameter hemisphere
                   fibre = "QP400-2-SR-BX 00S-004728-16")

saveRDS(D7_dome.ls, file = "./data-raw/calibrations-MAYP11278/d7-d-2021.rds")
