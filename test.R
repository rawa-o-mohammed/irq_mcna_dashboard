library(dplyr)
library(xlsx)
library(sf)                     # vector data tools
library(lwgeom)
library(raster)                 # raster data tools
library(leaflet)                # plot interactive maps
library(geojsonio)              # deal with geojson layers
library(spatialEco)             # calculate zonal statistics
library(rmapshaper)             # tools for simplifying polygons
library(HatchedPolygons)        # hatched polygon patterns with Leaflet
library(readxl)
library(readr)
library(openxlsx)



# read in MCNA datasets for each pop group and gender hhh
#female
# returnee
female_returnee_df <-
  read.csv(
    "csv_data/datasets/female_returnee.csv",
    na.strings = c("NA", "#N/A", "N/A"),
    encoding = "UTF-8"
  )
female_returnee_df <-
  female_returnee_df %>% rename_with(~ paste("Female_Returnees", .x, sep = "_"))

#in camp IDPs
female_in_camp_df <-
  read.csv(
    "csv_data/datasets/female_idp_in_camp.csv",
    na.strings = c("NA", "#N/A", "N/A"),
    encoding = "UTF-8"
  )
female_in_camp_df <-
  female_in_camp_df %>% rename_with(~ paste("Female_In_Camp", .x, sep = "_"))

#Out of camp IDPs
female_out_camp_df <-
  read.csv(
    "csv_data/datasets/female_idp_out_camp.csv",
    na.strings = c("NA", "#N/A", "N/A"),
    encoding = "UTF-8"
  )
female_out_camp_df <-
  female_out_camp_df %>% rename_with(~ paste("Female_Out_Camp", .x, sep = "_"))


#male
# returnee
male_returnee_df <-
  read.csv(
    "csv_data/datasets/male_returnee.csv",
    na.strings = c("NA", "#N/A", "N/A"),
    encoding = "UTF-8"
  )
male_returnee_df <-
  male_returnee_df %>% rename_with(~ paste("male_Returnees", .x, sep = "_"))

#in camp IDPs
male_in_camp_df <-
  read.csv(
    "csv_data/datasets/male_idp_in_camp.csv",
    na.strings = c("NA", "#N/A", "N/A"),
    encoding = "UTF-8"
  )
male_in_camp_df <-
  male_in_camp_df %>% rename_with(~ paste("male_In_Camp", .x, sep = "_"))

#Out of camp IDPs
male_out_camp_df <-
  read.csv(
    "csv_data/datasets/male_idp_out_camp.csv",
    na.strings = c("NA", "#N/A", "N/A"),
    encoding = "UTF-8"
  )
male_out_camp_df <-
  male_out_camp_df %>% rename_with(~ paste("male_Out_Camp", .x, sep = "_"))

all_df <- cbind(female_returnee_df, female_in_camp_df, female_out_camp_df, male_returnee_df, male_in_camp_df, male_out_camp_df)
all_df <- all_df[,] * 100

write.csv(all_df, "csv_data/datasets/national_popgroup_genderhhh.csv", row.names = FALSE)


# read in MCNA datasets for each pop group
# returnee
returnee_df <-
  read.csv(
    "csv_data/datasets/mcna_returnee_results.csv",
    na.strings = c("NA", "#N/A", "N/A"),
    encoding = "UTF-8"
  )
returnee_df <-
  returnee_df %>% rename_with(~ paste("Returnees", .x, sep = "_"))
colnames(returnee_df)[1:2] <- c("ADM1_EN", "ADM2_EN")

# in camp IDP
idp_out_camp_df <-
  read.csv(
    "csv_data/datasets/mcna_idp_out_camp_results.csv",
    na.strings = c("NA", "#N/A", "N/A"),
    encoding = "UTF-8"
  )
idp_out_camp_df <-
  idp_out_camp_df %>% rename_with(~ paste("Out_Camp", .x, sep = "_"))
colnames(idp_out_camp_df)[1:2] <- c("ADM1_EN", "ADM2_EN")

# out of camp IDP
idp_in_camp_df <-
  read.csv(
    "csv_data/datasets/mcna_idp_in_camp_results.csv",
    na.strings = c("NA", "#N/A", "N/A"),
    encoding = "UTF-8"
  )
idp_in_camp_df <-
  idp_in_camp_df %>% rename_with(~ paste("In_Camp", .x, sep = "_"))
colnames(idp_in_camp_df)[1:2] <- c("ADM1_EN", "ADM2_EN")



# ###################  DISTRICTS #############
# read in district shapefile layer
irq_dist <-
  st_read("spatial_data/admin/irq_admbnda_adm2_cso_20190603.shp",
          options = "ENCODING=UTF-8")

############################################################################################
########### join data to district shapefile
irq_dist_data <-
  left_join(irq_dist, returnee_df, by = "ADM2_EN")

irq_dist_data <-
  left_join(irq_dist_data, idp_out_camp_df, by = "ADM2_EN")

irq_dist_data <-
  left_join(irq_dist_data, idp_in_camp_df, by = "ADM2_EN")

# summarize coverage for full assessment
irq_dist_data <- irq_dist_data %>%
  mutate(
    All_indicative = case_when(
      Returnees_indicative == 0 | Out_Camp_indicative == 0 ~ 0,
      Returnees_indicative == 1 &
        is.na(In_Camp_indicative) ~ 1,
      Out_Camp_indicative == 1 &
        is.na(In_Camp_indicative) ~ 1,
      Returnees_indicative == 1 &
        !is.na(In_Camp_indicative) ~ 2,
      Out_Camp_indicative == 1 &
        !is.na(In_Camp_indicative) ~ 2,
      In_Camp_indicative == 1 &
        is.na(Out_Camp_indicative) &
        is.na(Returnees_indicative) ~ 1,
      TRUE ~ NA_real_
    )
  )
irq_dist_data <-
  irq_dist_data[, colSums(is.na(irq_dist_data)) != nrow(irq_dist_data)]

write_excel_csv(irq_dist_data, "csv_data/datasets/district_popgroup.csv ")


















