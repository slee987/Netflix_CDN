# =============================================================================
# Netflix CDN Panel Processing
# =============================================================================
# Structure:
#   BLOCK 1: Lib& Data Load
#   BLOCK 2: Spatial Info Mapping (DMA, Climate Zone, State)
#   BLOCK 3: Panel 
#   BLOCK 4: Imputation
#   BLOCK 5: Monthly Panel
#   BLOCK 6: Variables forDiD
#   BLOCK 7: Matching
# =============================================================================


# ── BLOCK 1: Lib & Data Load ────────────────────────────────────────
# 
library(arrow)
library(data.table)
library(dplyr)
library(lubridate)
library(sf)
library(tigris)
library(stringr)
library(zoo)

# 
df <- read_feather("C:/Users/Slee987/Downloads/final_panel_total4.feather")
setDT(df)

# Date
df[, odate               := as.Date(date, tz = "UTC")]
df[, nearest_install_date := as.Date(nearest_install_date, tz = "UTC")]


# ── BLOCK 2: Spatial Info Mapping ───────────────────────────────────────────────────
#  DMA / Climate Zone / State mapping → pixel_master
# 

# Additional Data
dma    <- fread("//Client/G:/My Drive/DataCen/Data/dmatmep.csv")
dma_sf <- st_as_sf(dma, wkt = "geometry", crs = 4326)

cz    <- fread("C:/Users/Slee987/Downloads/climate_zones.csv")
czmap <- st_read("C:/Users/Slee987/Downloads/cb_2018_us_county_5m.shp")

cz_processed <- cz[, .(
  GEOID = paste0(str_pad(`State FIPS`, 2, pad = "0"),
                 str_pad(`County FIPS`, 3, pad = "0")),
  `IECC Climate Zone`,
  `IECC Moisture Regime`
)]
cz_final_map <- czmap %>%
  left_join(cz_processed, by = "GEOID") %>%
  st_transform(4326)

# Unique Pixels
unique_pixels_sf <- unique(df[, .(pixel_id_hash, longitude, latitude)]) %>%
  .[!is.na(longitude) & !is.na(latitude)] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# (1) DMA mapping
nearest_dma_idx  <- st_nearest_feature(unique_pixels_sf, dma_sf)
unique_pixels_sf <- bind_cols(
  unique_pixels_sf,
  dma_sf[nearest_dma_idx, ] %>% st_drop_geometry()
)

# (2) Climate Zone mapping (intersection → nearest fallback)
unique_pixels_sf <- st_join(
  unique_pixels_sf,
  cz_final_map[, c("IECC Climate Zone", "IECC Moisture Regime")]
)
na_idx <- which(is.na(unique_pixels_sf$`IECC Climate Zone`))
if (length(na_idx) > 0) {
  nearest_cz_idx <- st_nearest_feature(unique_pixels_sf[na_idx, ], cz_final_map)
  unique_pixels_sf$`IECC Climate Zone`[na_idx]    <- cz_final_map$`IECC Climate Zone`[nearest_cz_idx]
  unique_pixels_sf$`IECC Moisture Regime`[na_idx] <- cz_final_map$`IECC Moisture Regime`[nearest_cz_idx]
}

# (3) US State mapping (intersection → nearest fallback)
us_states <- states(cb = TRUE, resolution = "20m") %>%
  st_transform(4326) %>%
  select(STUSPS, NAME)

unique_pixels_sf <- st_join(unique_pixels_sf, us_states)
na_state_idx <- which(is.na(unique_pixels_sf$STUSPS))
if (length(na_state_idx) > 0) {
  nearest_state_idx <- st_nearest_feature(unique_pixels_sf[na_state_idx, ], us_states)
  unique_pixels_sf$STUSPS[na_state_idx] <- us_states$STUSPS[nearest_state_idx]
  unique_pixels_sf$NAME[na_state_idx]   <- us_states$NAME[nearest_state_idx]
}

# pixel_master 
pixel_master <- as.data.table(unique_pixels_sf)
pixel_master[, geometry := NULL]
pixel_master <- unique(pixel_master, by = "pixel_id_hash")

#  Managing Memory
rm(cz, cz_final_map, cz_processed, czmap, us_states,
   unique_pixels_sf, dma, dma_sf)


# ── BLOCK 3: Balanced Paenl───────────────────────────────────────────────────


#  pixel × date combination
grid <- CJ(
  pixel_id_hash = unique(df$pixel_id_hash),
  odate         = unique(df$odate)
)

# LST merge
df_full <- merge(grid, df, by = c("pixel_id_hash", "odate"), all.x = TRUE)
setorder(df_full, pixel_id_hash, odate, `LST(10am_Terra)`, na.last = TRUE)
df_full <- unique(df_full, by = c("pixel_id_hash", "odate"))

# pixel_master join
df_full[, c("STUSPS", "NAME") := NULL]
df_full <- merge(df_full, pixel_master, by = "pixel_id_hash", all.x = FALSE)

# delete unnecessary columns
cols_to_remove <- c(
  "nearest_install_date", "index_right", "treatment_status",
  "is_treated_group", "dist_to_cdn", "pixel_idx",
  "dc_count", "has_netflix", ".geo", "poly_id", "system:index"
)
df_full[, (intersect(cols_to_remove, names(df_full))) := NULL]

#Rename LST Columns 
setnames(df_full,
         old  = c("LST(10am_Terra)", "LST(10pm_Terra)", "LST(1pm_Aqua)", "LST(1am_Aqua)"),
         new  = c("lst_10am",        "lst_22pm",         "lst_13pm",      "lst_01am"),
         skip_absent = TRUE
)


# ── BLOCK 4: Imputation ─────────────────────────────────────────


lst_cols <- c("lst_10am", "lst_13pm", "lst_22pm", "lst_01am")

# bias(Impute)
fill_sensor <- function(dt, col1, col2, group_var) {
  bias_name <- paste0("bias_", col1)
  dt[, (bias_name) := mean(get(col1) - get(col2), na.rm = TRUE), by = group_var]
  dt[is.na(get(col1)) & !is.na(get(col2)), (col1) := get(col2) + get(bias_name)]
  dt[is.na(get(col2)) & !is.na(get(col1)), (col2) := get(col1) - get(bias_name)]
  dt[, (bias_name) := NULL]
}

# State + cross-sensor  (night / day)
fill_sensor(df_full, "lst_10am", "lst_13pm", "STUSPS")
fill_sensor(df_full, "lst_22pm", "lst_01am", "STUSPS")

# linear imputation within a pixel ( gap = 3)
df_full[, (lst_cols) := lapply(.SD, function(x) {
  if (sum(!is.na(x)) < 2) return(x)
  zoo::na.approx(x, na.rm = FALSE, maxgap = 3)
}), by = pixel_id_hash, .SDcols = lst_cols]

# lag dep vars (1~3)
for (col in lst_cols) {
  lag_names <- paste0(col, "_lag", 1:3)
  df_full[, (lag_names) := shift(.SD, n = 1:3, type = "lag"),
          by = pixel_id_hash, .SDcols = col]
}

# STUSPS 
df_full[, STUSPS := first(na.omit(STUSPS)), by = pixel_id_hash]


# ── BLOCK 5: df_monthly ──────────────────────────────────────────



# Daily avg LST 
df_full[, daily_mean_temp := rowMeans(.SD, na.rm = TRUE),
        .SDcols = c("lst_10am", "lst_13pm", "lst_22pm", "lst_01am")]
df_full[, daily_day   := rowMeans(.SD, na.rm = TRUE),
        .SDcols = c("lst_10am", "lst_13pm")]
df_full[, daily_night := rowMeans(.SD, na.rm = TRUE),
        .SDcols = c("lst_22pm", "lst_01am")]

# Aggregate - Month
df_full[, month_id := floor_date(odate, "month")]

df_monthly <- df_full[, .(
  lst_daily = mean(daily_mean_temp, na.rm = TRUE),
  lst_day   = mean(daily_day,       na.rm = TRUE),
  lst_night = mean(daily_night,     na.rm = TRUE)
), by = c("pixel_id_hash", "month_id")]

# Covariates pixel (pre treatment)
pixel_vars <-  c(
  "STUSPS", "NAME", "IECC Climate Zone",
  "elevation", "dist_to_water", "pop_density",
  "NDVI_2010", "imperv_mean_1km",
  "ndc_counts", "earliest_nflx_date",
  "netpop", "ever_treated"
)
target_vars <- intersect(pixel_vars, names(df_full))

pixel_info <- unique(df_full[, c("pixel_id_hash", target_vars), with = FALSE])
pixel_info <- pixel_info[order(pixel_id_hash, elevation, na.last = TRUE)]
pixel_info <- unique(pixel_info, by = "pixel_id_hash")

df_monthly <- merge(df_monthly, pixel_info, by = "pixel_id_hash", all.x = TRUE)

# Kelvin → Celsius  (scale factor 0.02)
df_monthly[, `:=`(
  temp      = lst_daily * 0.02 - 273.15,
  daytemp   = lst_day   * 0.02 - 273.15,
  nighttemp = lst_night * 0.02 - 273.15
)]


time_invariant <- c("elevation", "dist_to_water", "pop_density",
                    "NDVI_2010", "imperv_mean_1km")
for (v in time_invariant) {
  if (v %in% names(df_monthly))
    df_monthly[, (v) := first(na.omit(get(v))), by = pixel_id_hash]
}
time_varying_vars <- c("dc_counts", "total_asn_count", "total_mbps", "nflx_mbps")

tv_monthly <- df_full[, lapply(.SD, mean, na.rm = TRUE),
                      by = .(pixel_id_hash, month_id),
                      .SDcols = intersect(time_varying_vars, names(df_full))]

df_monthly <- merge(df_monthly, tv_monthly,
                    by = c("pixel_id_hash", "month_id"), all.x = TRUE)

# 확인
summary(df_monthly$nflx_mbps)

# ── BLOCK 6: DiD 변수 생성 ───────────────────────────────────────────────────
# [기존] earliest_nflx_date / treat_month / rt_month / is_post / treated_post
# 변경: 중복 연산 제거, 순서 정리

# earliest_nflx_date 정제 (2011 이전 제거)
df_monthly[, earliest_nflx_date := as.IDate(earliest_nflx_date)]
df_monthly[earliest_nflx_date < as.IDate("2011-01-01"), earliest_nflx_date := NA]

# ever_treated / first_treat_month
df_monthly[, ever_treated := as.integer(any(!is.na(earliest_nflx_date))),
           by = pixel_id_hash]
df_monthly[, treat_month := floor_date(earliest_nflx_date, "month")]
df_monthly[, first_treat_month := {
  x <- treat_month
  if (all(is.na(x))) as.IDate(NA) else min(x, na.rm = TRUE)
}, by = pixel_id_hash]

# treated (time-varying: 1 if Netflix present in that month)
df_monthly[, treated := as.numeric(!is.na(earliest_nflx_date))]

# relative month (rt_month)
df_monthly[, rt_month := NA_integer_]
df_monthly[ever_treated == 1,
           rt_month := (year(month_id) - year(treat_month)) * 12 +
             (month(month_id) - month(treat_month))
]

# rt_month_cap / is_post / treated_post
df_monthly[, rt_month_cap  := pmax(pmin(rt_month, 12), -12)]
df_monthly[, is_post        := as.integer(!is.na(rt_month) & rt_month >= 0)]
df_monthly[, treated_post   := as.integer(ever_treated == 1 & is_post == 1)]

# climate_zone factor
df_monthly[, climate_zone := as.factor(`IECC Climate Zone`)]

# LST lag
setDT(df_monthly)
df_monthly <- df_monthly %>%
  arrange(pixel_id_hash, month_id) %>%
  group_by(pixel_id_hash) %>%
  mutate(
    temp_lag1 = lag(temp, 1),
    temp_lag2 = lag(temp, 2),
    temp_lag3 = lag(temp, 3)
  ) %>%
  ungroup()
setDT(df_monthly)


# ── BLOCK 7: Matching data ────────────────────────────────────────

#

df_monthly[, pre_month := max(month_id[!is.na(rt_month_cap) & rt_month_cap < 0],
                              na.rm = TRUE),
           by = pixel_id_hash]

df_match <- df_monthly %>%
  filter(month_id == pre_month) %>%
  select(
    pixel_id_hash,
    ever_treated,
    elevation,
    dist_to_water,
    pop_density,
    NDVI_2010,
    imperv_mean_1km,
    STUSPS,
    temp,
    temp_lag1,
    temp_lag2,
    temp_lag3
  )

cat("=== Done ===\n")
cat("df_monthly:", nrow(df_monthly), "Rows,",
    uniqueN(df_monthly$pixel_id_hash), "Pixel\n")
cat("df_match:  ", nrow(df_match),   "Rows\n")
cat("treated pixel:", df_monthly[ever_treated==1, uniqueN(pixel_id_hash)], "\n")

