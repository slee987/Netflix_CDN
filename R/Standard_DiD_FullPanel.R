# =============================================================================
# Step 1: Standard DiD (Full Panel, No Matching)
# =============================================================================
# Baseline estimate using full df_monthly
# nflx = ever_treated * is_post (time-varying treatment indicator)
# FE: pixel_id_hash (unit) + month_id^STUSPS (state x time)
# SE: clustered at pixel level
library(fixest)
df_monthly[, post := as.integer(month_id >= first_treat_month)]
df_monthly[is.na(first_treat_month), post := 0L]
df_monthly[, nflx := as.integer(ever_treated == 1 & post == 1)]
df_monthly<- df_monthly %>% mutate(rt_month=ifelse(treated==1,rt_month,-1))
step1 <- feols(
  c(temp, nighttemp, daytemp) ~ nflx  |
    pixel_id_hash + month_id^STUSPS,
  cluster = ~pixel_id_hash,
  data    = df_monthly
)

etable(step1,
       dict = c(
         temp      = "LST (All)",
         nighttemp = "LST (Night)",
         daytemp   = "LST (Day)",
         nflx      = "Netflix OCA",
         prism_ppt = "Precipitation"
       ),
       title = "Step 1: Standard DiD (Full Panel)"
)

# Event study (full panel)

step1_all <- feols(
  temp ~ i(rt_month, ref = -1)  |
    pixel_id_hash + month_id^STUSPS,
  cluster = ~pixel_id_hash,
  data    = df_monthly[abs(rt_month) <= 24]
)
iplot(step1_all,
      main = "Step 1: Event Study (Full Panel, Average LST)",
      xlab = "Months relative to treatment"
)
abline(h = 0, lty = 2)

step1_day <- feols(
  daytemp ~ i(rt_month, ref = -1)  |
    pixel_id_hash + month_id^STUSPS,
  cluster = ~pixel_id_hash,
  data    = df_monthly[abs(rt_month) <= 24]
)
iplot(step1_day,
      main = "Step 1: Event Study (Full Panel, Daytime LST)",
      xlab = "Months relative to treatment"
)
abline(h = 0, lty = 2)


step1_night <- feols(
  nighttemp ~ i(rt_month, ref = -1)  |
    pixel_id_hash + month_id^STUSPS,
  cluster = ~pixel_id_hash,
  data    = df_monthly[abs(rt_month) <= 24]
)
iplot(step1_night,
      main = "Step 1: Event Study (Full Panel, Nighttime LST)",
      xlab = "Months relative to treatment"
)
abline(h = 0, lty = 2)
