required_packages <- c("tidyverse", "sf", "conflicted", "randomForest", "janitor")
suppressWarnings(
  install_and_load_packages(required_packages)
)


select <- dplyr::select
filter <- dplyr::filter


permits_bg <- st_read(final_dataset_path, quiet = TRUE)

### organize data

# test 
t_train <- filter(permits_bg %>% select(-c(mapname, geoid10)), year < 2022)
t_test <- filter(permits_bg %>% select(-c(mapname, geoid10)), year == 2022)

## validate
v_train <- filter(permits_bg %>% select(-c(mapname, geoid10)), year < 2023)
v_test <- filter(permits_bg %>% select(-c(mapname, geoid10)), year == 2023)

## predict
p_train <- filter(permits_bg %>% select(-c(mapname, geoid10)), year < 2024)
p_predict <- filter(permits_bg %>% select(-c(mapname, geoid10)), year == 2024)

keep_cols <- c("permits_count",
               "abs_error",
               "district",
               "total_pop",
               "med_inc",
               "percent_nonwhite",
               "percent_renters",
               "rent_burden",
               "ext_rent_burden"
               )



### ols----------------------------------------------
ols <- lm(permits_count ~ ., data = st_drop_geometry(t_train))

ols_preds <- predict(ols, t_test)
ols_preds <- cbind(t_test, ols_preds) %>%
  mutate(abs_error = abs(permits_count - ols_preds),
         pct_error = abs_error / permits_count) %>% 
  select(ols_preds, all_of(keep_cols))

# save results
st_write(ols_preds, 'data/model_outputs/ols_preds.geojson')

### rf test----------------------------------------
rf_test <- randomForest(permits_count ~ ., 
                   data = st_drop_geometry(t_train),
                   importance = TRUE, 
                   na.action = na.omit)

rf_test_preds <- predict(rf_test, t_test)
rf_test_preds <- cbind(t_test, rf_test_preds)%>%
                    mutate(abs_error = abs(permits_count - rf_test_preds),
                           pct_error = abs_error / (permits_count + 0.0001))%>% 
                    select(rf_test_preds, all_of(keep_cols))

st_write(rf_test_preds, 'data/model_outputs/rf_test_preds.geojson')


### rf validate---------------------------------------
rf_val <- randomForest(permits_count ~ ., 
                        data = st_drop_geometry(v_train),
                        importance = TRUE, 
                        na.action = na.omit)

rf_val_preds <- predict(rf_val, v_test)
rf_val_preds <- cbind(v_test, rf_val_preds)%>%
  mutate(abs_error = abs(permits_count - rf_val_preds),
         pct_error = abs_error / (permits_count + 0.0001))%>% 
  select(rf_val_preds, all_of(keep_cols))

st_write(rf_val_preds, 'data/model_outputs/rf_val_preds.geojson')


### rf project----------------------------------
rf_proj <- randomForest(permits_count ~ ., 
                       data = st_drop_geometry(p_train),
                       importance = TRUE, 
                       na.action = na.omit)

rf_proj_preds <- predict(rf_proj, p_predict)
rf_proj_preds <- cbind(p_predict, rf_proj_preds)%>%
  mutate(abs_error = abs(permits_count - rf_proj_preds),
         pct_error = abs_error / (permits_count + 0.0001))%>% 
  select(rf_proj_preds, all_of(keep_cols))

st_write(rf_proj_preds, 'data/model_outputs/rf_proj_preds.geojson')
