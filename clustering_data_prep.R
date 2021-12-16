# Package names
packages <- c("haven", "dplyr", "tidyr", "fpc", "NbClust", "factoextra")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# We load the original Czech dataset (in SPSS format) from a local directory.
data <- zap_labels(haven::read_sav(file = "INTERNATIONAL DATASET_12 Oct_final.sav"))


data <- data %>%
  mutate(Q49_1_rec = recode(Q49_1,
                            `1` = 0,
                            `2` = 1,
                            `3` = 2,
                            `4` = 3),
         Q49_2_rec = recode(Q49_2,
                            `1` = 0,
                            `2` = 1,
                            `3` = 2,
                            `4` = 3),
         Q49_3_rec = recode(Q49_3,
                            `1` = 0,
                            `2` = 1,
                            `3` = 2,
                            `4` = 3),
         Q49_4_rec = recode(Q49_4,
                            `1` = 0,
                            `2` = 1,
                            `3` = 2,
                            `4` = 3),
         Q49_5_rec = recode(Q49_5,
                            `1` = 0,
                            `2` = 1,
                            `3` = 2,
                            `4` = 3),
         Q49_6_rec = recode(Q49_6,
                            `1` = 0,
                            `2` = 1,
                            `3` = 2,
                            `4` = 3),
         Q49_7_rec = recode(Q49_7,
                            `1` = 0,
                            `2` = 1,
                            `3` = 2,
                            `4` = 3),
         Q49_8_rec = recode(Q49_8,
                            `1` = 0,
                            `2` = 1,
                            `3` = 2,
                            `4` = 3),
         PHQ8 = Q49_1_rec + Q49_2_rec + Q49_3_rec + Q49_4_rec + Q49_5_rec + Q49_6_rec + Q49_7_rec + Q49_8_rec)

data_subset <- data %>% 
  filter(Q3 %in% c(1,2), 
         Country != 5) %>% 
  transmute(q01_gender = Q3, 
            q02_age = Q4, 
            q18_02_soc_media = replace_na(Q21_2, 0),
            q20_public_info = Q23,
            q36_econ_worry = Q39,
            q35_01_contact_close_family = Q38_1,
            q35_03_contact_friends = Q38_3,
            q38_alcohol = Q42,
            q47_self_reporting_health = Q50,
            q49_health_limitations = Q52,
            PHQ8) %>% 
  na.omit() %>% 
  scale()

saveRDS(data_subset, "clustering_matrix.rds")

matrix_subset <- readRDS("clustering_matrix.rds")

#identify all factor columns
# x <- sapply(, is.factor)
# 
# data_subset[ , x] <- sapply(data_subset[ , x], MARGIN = 2, as.numeric)
# 
# data_scaled <- sapply(data_subset, as.integer) %>% scale()

# Calculate the matrix of distance, we can choose from many methods

distance_matrix <- get_dist(matrix_subset, method = "euclidean")

fviz_dist(distance_matrix, gradient = list(low = "blue", mid = "white", high = "red"))

fviz_nbclust(distance_matrix, pam, method = "wss")
fviz_nbclust(distance_matrix, pam, method = "silhouette")
fviz_nbclust(distance_matrix, pam, method = "gap_stat")

resnumclust <- NbClust(matrix_subset, distance = "euclidean", min.nc = 2, max.nc = 10, method = "median", index = "alllong")

resnumclust

fviz_nbclust(resnumclust)

# 3 seems to be the suggested number of clusters

set.seed(4167)

pam3 <- pam(data_scaled, 3)

