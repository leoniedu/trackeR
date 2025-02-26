# library(rStrava)
# app_name <- 'yacht_vaa' # chosen by user
# app_client_id  <- '150108' # an integer, assigned by Strava
# app_secret <- 'abffe697799707057f46c514eef487f539c3ec35' # an alphanumeric secret, assigned by Strava
#
# # create the authentication token
# stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))
#
# my_acts <- get_activity_list(stoken, after = as.Date('2020-12-31'))
# compiled <- compile_activities(my_acts)
#
# strms_data <- get_activity_streams(my_acts, stoken)
# strms_data_c <- left_join(compiled,strms_data, by="id", suffix = c("_compiled", ""))%>%
#   mutate(heartrate=if_else(heartrate==0, NA_real_, heartrate))
#
#
#
# athl_fun('2837007', trace = FALSE)

library(trackeR)
library(dplyr)
library(ggplot2)
library(sf)
r <- read_directory("~/Downloads/export_23457887/activities/", sport = "cycling", aggregate=TRUE, session_threshold=1)
rt <- threshold(r)%>%
  threshold(variable="speed", lower=0, upper=20, sport="cycling")

rdata <- vector(mode="list", length=length(rt))

for (i in seq_along(r)) {
  rdata[[i]] <- fortify(rt[[i]], names = "date")%>%
  filter(lubridate::year(date)>=2024)%>%
  mutate(session=i)
}
rdata <- bind_rows(rdata)

# res <- list()
# for (tipo in c("oc1", "oc6")) {
#   canoas <- dir(file.path("data-raw/paddling/", tipo))
#   for (canoa in canoas) {
#     r <- read_directory(file.path("data-raw/paddling/", tipo, canoa), sport = "cycling")
#     #attr(r, "tipo") <- tipo
#     #attr(r, "canoa") <- canoa
#     #res[[paste(tipo,canoa)]] <- r
#     session <- 1
#     for (i in seq_along(r)) {
#       r[[i]] <- fortify.zoo(r[[i]], names = "date")%>%
#         mutate(session=paste(as.Date(date), session), canoa=canoa, tipo=tipo)
#       session <- session+1
#     }
#     res[[paste(canoa,tipo)]] <- r
#   }
# }


library(lubridate)
library(slider)
k=3

d2b <- function(d) {
  if_else(d>180,360-d,  d)
}

r <- function(x, d) {
  newx <- x-d
  newx <- if_else(newx< -180, newx+360, newx)
  newx <- if_else(newx > 180, newx-360,newx)
  ## 1 along the tide direction in one way
  ## -1 along the tide direction in the other way
  1-abs(newx/90)
}

ptt0 <- res%>%
  bind_rows()%>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)%>%
  group_by(session)%>%
  arrange(date)%>%
  dplyr::arrange(date)%>%
  #group_by(round(as.numeric(date)/10))%>%
  #mutate(seconds=diff(range(date)))%>%
  #summarise(across(c(date, heart_rate, speed, cadence_running, cadence_cycling, seconds), \(x) median(x, na.rm=TRUE)), distance_0=diff(range(distance)))%>%
  #mutate(distance=cumsum(distance_0))%>%
  orce::add_coordinates(lon="longitude", lat="latitude")%>%
  mutate(distance2=c(NA_real_, cumsum(as.numeric(geodist::geodist(cbind(longitude, latitude), cbind(dplyr::lag(longitude), dplyr::lag(latitude)), measure = "geodesic", paired = TRUE))%>%na.omit())))%>%
  ungroup%>%
  mutate(bearing=c(lwgeom::st_geod_azimuth(.),
                   units::set_units(NA_real_, "radians"))%>%units::set_units("degrees")%>%as.numeric())%>%
  dplyr::mutate(distance_line_k=slider::slide_index2_dbl(.x = longitude, .y = latitude,  .before = lubridate::minutes (k), .complete = TRUE, .f = function(x,y) {
    as.numeric(geodist::geodist(c(lon=first(x), lat=first(y)), c(lon=last(x), lat=last(y)), measure = "geodesic"))
  }
  , .i = date))%>%
  dplyr::mutate(distance_k=slider::slide_index_dbl(.x = distance, .before = lubridate::minutes (k), .complete = TRUE, .f = function(z) last(z)-first(z), .i = date))%>%
  dplyr::mutate(n_k=slider::slide_index_dbl(.x = distance, .before = lubridate::minutes (k), .complete = TRUE, .f = function(z) length(z), .i = date))%>%
  mutate(across(ends_with("_k"), ~if_else(n_k<(k*60),NA_real_, .x)))%>%
  mutate(steer_eff=distance_line_k/distance_k)

date_min <- floor(as.numeric(min(ptt0$date)))-60*60
date_max <- ceiling(as.numeric(max(ptt0$date)))+60*60

download_boia <- (!exists("boia"))
if (!download_boia) {
  download_boia <- (min(boia$date)>min(ptt0$date))|
  (max(boia$date)<max(ptt0$date))
}
if (download_boia) {
  meteorologicos <- jsonlite::read_json(glue::glue('https://simcosta.furg.br/api/intrans_data?boiaID=515&type=json&time1={date_min}&time2={date_max}&params=Avg_Wnd_Dir_N,Gust_Sp,Avg_Dew,Avg_Air_Press,Avg_Sol_Rad,Avg_Air_Tmp,Avg_Hmt,Avg_Wnd_Sp'), simplifyVector = TRUE)
  ## C_Avg_Spd não está especificado no link api gerado
  oceanograficos <- jsonlite::read_json(glue::glue('https://simcosta.furg.br/api/intrans_data?boiaID=515&type=json&time1={date_min}&time2={date_max}&params=H10,Havg,Hsig,HM0,Avg_Wv_Dir_N,Hmax,ZCN,Tp5,Tavg,T10,Tsig,Avg_Wv_Spread_N,Tp,Avg_Sal,Avg_W_Tmp1,Avg_W_Tmp2,Avg_CDOM,Avg_Chl,Avg_DO,Avg_Turb,C_Avg_Dir_N,tidbits_temp,C_Avg_Spd,C_Cell_2_North_Speed'), simplifyVector = TRUE)
  #https://simcosta.furg.br/api/intrans_data?boiaID=515&type=json&time1=1740279600&time2=1740366000&params
  correntes <- jsonlite::read_json(glue::glue('https://simcosta.furg.br/api/intrans_data?boiaID=515&type=json&time1={date_min}&time2={date_max}&params=perfil_correntes&extras=dir_n'), simplifyVector = TRUE)
  boia_0 <- full_join(correntes, oceanograficos)%>%full_join(meteorologicos)
  boia <- boia_0%>%
    transmute(#timestamp,
      date=lubridate::ymd_hms(timestamp, tz="UTC"),
      wind_direction=Avg_Wnd_Dir_N,
      wind_speed=Avg_Wnd_Sp,
      air_temperature=Avg_Air_Tmp,
      wave_height=Havg,
      wave_period=Tavg,
      wave_direction=Avg_Wv_Dir_N,
      ## oceanografica
      tide_speed_kmh=
        ## in milimiter/second
        ## so * 60 (minute) * 60 (hour)
        ## /1000 (meters)
        ## /1000 (km)
        C_Avg_Spd*60*60/1000/1000,
      tide_direction_1=as.numeric(C_Avg_Dir_N),
      ## correntes
      #tide_direction_1=as.numeric(`Avg_Cell(001)_dir_n`),
      tide_direction_2=as.numeric(`Avg_Cell(002)_dir_n`),
      tide_direction_avg=(tide_direction_1+tide_direction_2)/2)
}


ptt <- ptt0%>%
  bind_rows(boia)%>%
  arrange(date)%>%
  tidyr::fill(all_of(names(boia)[-1]), .direction = "downup")%>%
  mutate(#rel_direction_1=r(bearing, 30),
         rel_direction_tide_avg=r(bearing, tide_direction_avg),
         rel_direction_tide1=r(bearing, tide_direction_1),
         rel_direction_tide2=r(bearing, tide_direction_2),
         rel_direction_tide_f=r(bearing, 30),
         rel_direction_wind=r(bearing, wind_direction),
         rel_direction_wave=r(bearing, wave_direction)
         )

oc1 <- ptt%>%
  filter(tipo%in%c("oc1"))%>%
  group_by(session)%>%
  filter(!is.na(speed))%>%
  mutate(time=date-min(date),
         time_min=time-60*k)%>%
  mutate(speed_line_k=distance_line_k/k*60/1000)%>%
  ungroup()



m <- lm(speed_line_k~
          wave_period+
          wave_height*rel_direction_wave+
          #dplyr::lag(speed)+
          rel_direction_tide2*tide_speed_kmh+
          #rel_direction_tide1*tide_speed_kmh+
          rel_direction_wind*wind_speed+
          factor(canoa)*(
            heart_rate+
              #dplyr::lag(heart_rate,1)+
              #dplyr::lag(cadence_cycling)+
              cadence_cycling),data=oc1)
summary(m)

oc1$speed_line_k_hat <- predict(m, newdata = oc1)

top <- oc1%>%
  group_by(session, rel_direction_tide_f=cut(rel_direction_tide2, c(-1,-.5,.5,1)))%>%
  arrange(desc(distance_line_k))%>%
  filter(!is.na(rel_direction_tide_f))%>%
  #mutate(r=1:n())%>%
  slice(1)


ggplot(aes(x=rel_direction_tide_f, color=factor(session), group=session), data=top)+   geom_line(aes(y=speed_line_k))+
  geom_line(aes(y=speed_line_k_hat), linetype=2)#+
#  geom_line(aes(y=speed_line_k-speed_line_k_hat), linetype=4)

ggplot(aes(x=rel_direction_tide_f, color=factor(session), group=session), data=top)+
  geom_line(aes(y=speed_line_k-speed_line_k_hat), linetype=4)



ggplot(data=oc1)+
  geom_point(aes(time, speed_line_k), color="black") +
  geom_line(aes(time, speed_line_k_hat), color="red", alpha=1) +
  facet_wrap(~session, scales="free_x")




oc1%>%group_by(session, canoa)%>%summarise(date=min(date))
#test_sessions <- c("2025-02-12 2", "2025-02-21 1")
test_sessions <- oc1%>%filter(canoa=="fenix")%>%pull(session)%>%unique
oc1_train <- oc1%>%filter(!session%in%test_sessions)
oc1_test <- oc1%>%filter(session%in%test_sessions)

m0 <- lm(speed~
          rel_direction_tide1*tide_speed_kmh +
          rel_direction_tide2*tide_speed_kmh +
          rel_direction_wind*wind_speed+
          #rel_direction_wind+
          (

            heart_rate+
              dplyr::lag(heart_rate,5)+
              dplyr::lag(cadence_cycling,5)+
              cadence_cycling),data=oc1_train)
oc1_test$speed_hat <- predict(m0, newdata = oc1_test)
oc1_train$speed_hat <- predict(m0, newdata = oc1_train)
ggplot(data=oc1_train)+
  geom_line(aes(time, speed), color="black", alpha=.5) +
  geom_line(aes(time, speed_hat), color="red", alpha=.5)

ggplot(data=oc1_test)+
  geom_line(aes(time, speed), color="black", alpha=.5) +
  geom_line(aes(time, speed_hat), color="red", alpha=.5) +
  facet_wrap(~session)


summary(lm(speed~
             #dplyr::lag(speed)+
             rel_direction_tide2+
             heart_rate+
             #dplyr::lag(heart_rate,1)+
             cadence_cycling,data=ptt))

stop()

summary(ptt$rel_direction)






summary(lm(speed~
             #dplyr::lag(speed)+
             rel_direction+
             heart_rate+
             #dplyr::lag(heart_rate,1)+
             cadence_cycling,data=ptt))

pttsum <- ptt%>%
  # dplyr::mutate(
  #   distance_line_k=slider::slide_index2_dbl(.x = longitude, .y = latitude,  .before = lubridate::minutes (k), .complete = TRUE, .f = function(x,y) {
  #     as.numeric(geodist::geodist(c(lon=first(x), lat=first(y)), c(lon=last(x), lat=last(y)), measure = "geodesic"))
  #   }
  #   , .i = date))%>%
  mutate(bearing_line_k=slider::slide_index2_dbl(.x = longitude, .y = latitude,  .before = lubridate::minutes (k), .complete = TRUE, .f = function(x,y) {
    lwgeom::st_geod_azimuth(st_sfc(st_point(c(first(x),first(y))), st_point(c(last(x),last(y))), crs = 4326))%>%units::set_units("degrees")%>%as.numeric()
  }
  , .i = date))%>%
  mutate(rel_direction_k=r(bearing_line_k,30))%>%
  ## t_k is k
  #dplyr::mutate(t_k=slider::slide_index_dbl(.x = date, .before = lubridate::minutes (k), .complete = TRUE, .f = function(z) as.numeric(diff(range(z))), .i = date))%>%
  mutate(v_k=(distance_line_k/k)*60/1000)%>%
  filter(!is.na(distance_line_k))%>%
  group_by(cut(rel_direction_k, c(-1,-.25,.25,1)))%>%
  arrange(desc(v_k))%>%
  slice(1)









ptt <- pt0[[1]]%>%fortify.zoo(names = "date")%>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)%>%
  dplyr::arrange(date) %>%
  mutate(bearing=c(lwgeom::st_geod_azimuth(.),
                   units::set_units(NA_real_, "radians"))%>%units::set_units("degrees")%>%as.numeric(),
         direction=if_else(bearing<0, 360+bearing, bearing))%>%
  dplyr::mutate(distance_slide5=slider::slide_index_dbl(.x = distance, .before = lubridate::minutes (k - 1), .complete = TRUE, .f = function(z) last(z)-first(z), .i = date))


  mutate()
  group_by(date(date), hour(date), minute(date))%>%
  summarise(direction=mean(direction),
            heart_rate=mean(heart_rate),
            cadence_cycling=mean(cadence_cycling),
            speed=(last(distance)-first(distance))*60/1000)

summary(lm(speed~
             dplyr::lag(speed,1)+
             direction+
             heart_rate+
             dplyr::lag(heart_rate,1)+
             cadence_cycling,data=ptt))

ggplot(aes(x=speed,y=direction), data=ptt) +
  geom_smooth(alpha=1/3)+
  geom_point()
+
  geom_path()
+
  scale_y_continuous(expand = expansion(0, 0),
                     limits = c(0, 360),
                     breaks = 0:3 * 90)+coord_radial(theta = "y", start=0)



plot(pt0,  what=c("speed", "pace", "heart_rate"))

pad_summary <- summary(pt0)
plot(pad_summary, group = c("total", "moving"),
     what = c("avgSpeed", "distance", "duration", "avgHeartRate"))


runsT <- threshold(pt0)
dp_runs <- distribution_profile(runsT, what = c("speed", "heart_rate"))
dp_runs_smooth <- smoother(dp_runs)
cp_runs <- concentration_profile(dp_runs_smooth)
plot(cp_runs, multiple = TRUE, smooth = FALSE)


## fit functional PCA
cp_PCA <- funPCA(cp_runs, what = "speed", nharm = 4)

## pick first 2 harmonics/principal components
round(cp_PCA$varprop, 2)

## [1] 0.66 0.25 0.06 0.02

## plot harmonics
plot(cp_PCA, harm = 1:2)
