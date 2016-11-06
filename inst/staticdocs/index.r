sd_section("Package overview", "",
           c("ooacquire-package")
)

sd_section("Data acquisition",
  "Functions related to acquisition of spectral data from array spectrometers",
  c("start_session",
    "end_session",
    "list_instruments",
    "get_oo_descriptor",
    "set_descriptor_wl",
    "set_descriptor_bad_pixs",
    "set_descriptor_nl",
    "set_descriptor_irrad_mult",
    "set_descriptor_integ_time",
    "set_descriptor_slit_fun",
    "get_oo_settings",
    "acq_settings",
    "acq_raw_spct",
    "acq_raw_mspct")
)

sd_section("Data processing", "",
  c("trim_counts",
    "bleed_nas",
    "linearize_counts",
    "skip_bad_pixs",
    "ref_correction",
    "maya_tail_correction",
    "uvb_corrections",
    "raw2cps",
    "merge_cps")
)

sd_section("File reading", "",
           c("read_oo_ssdata",
             "read_oo_pidata",
             "read_files2mspct",
             "map_oofile_header_rows",
             "oofile_data_rows",
             "set_oo_ssdata_settings",
             "set_oo_ssdata_descriptor",
             "merge_raw_mspct")
)

sd_section("High level method", "",
           c("raw2corr_cps",
             "s_irrad_corrected")
)

sd_section("Example data", "",
           c("maya_descriptor",
             "maya_ylianttila")
)
