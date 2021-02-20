# To do: 
# Add burned status as another (set of) predictor variables OR make a version 
# of fig 1 for each of the burn severity things (are interactions more 
# in less severely disturbed areas?)
# Add in block as a spatial scale so there are 4 spatial scales

# Exploration

library("tidyverse")
library("gbm")
library("caret")

# Maybe get rid of the unburned
# Maybe particular years are better or worse


# Load data ------------

bee_dat <- readxl::read_excel("DurneyBeeALLsev b vs u for VAR 10-31-18.xlsx")
plant_dat <- readxl::read_excel("DurneyFlwrALLsev_burkle b vs u 10-29-18.xlsx")

colnames(bee_dat) <- tolower(colnames(bee_dat))
colnames(plant_dat) <- tolower(colnames(plant_dat))

plant_dat <- plant_dat %>% 
  mutate(jdate = julian_date %>% substr(., 3, 5) %>% as.numeric(),
         yr = julian_date %>% substr(., 1, 2) %>% as.numeric(),
         mo = as.character(date) %>% substr(., 6,7),
         biweek = paste(as.character(date) %>% substr(., 6,7),
                        ifelse(as.numeric(as.character(date) %>% substr(., 9,10))>15, 1, 2)),
         flower_species = gsub("_", " ", plant_dat$flower_species, fixed = T))

bee_dat <- bee_dat %>% 
  mutate(jdate = julian_date %>% substr(., 3, 5) %>% as.numeric(),
         yr = julian_date %>% substr(., 1, 2) %>% as.numeric(),
         mo = as.character(date) %>% substr(., 3,5),
         biweek = paste(as.character(date) %>% substr(., 3,5),
                        ifelse(as.numeric(as.character(date) %>% substr(., 1,2))>15, 1, 2)),
         metanet = "metanet") %>% 
  mutate(metanet_yr = paste(metanet, yr, sep="_"),
         metanet_mo = paste(metanet, yr, mo, sep="_"),
         metanet_biweek = paste(metanet, yr, mo, biweek, sep="_"),
         metanet_day = paste(metanet, julian_date, sep="_"),
         
         location_yr = paste(location, yr, sep="_"),
         location_mo = paste(location, yr, mo, sep="_"),
         location_biweek = paste(location, yr, mo, biweek, sep="_"),
         location_day = paste(location, julian_date, sep="_"),
         
         combo_yr = paste(combo, yr, sep="_"),
         combo_mo = paste(combo, yr, mo, sep="_"),
         combo_biweek = paste(combo, yr, mo, biweek, sep="_"),
         combo_day = paste(combo, julian_date, sep="_"),
         
         transect_combo_yr = paste(transect_combo, yr, sep="_"),
         transect_combo_mo = paste(transect_combo, yr, mo, sep="_"),
         transect_combo_biweek = paste(transect_combo, yr, mo, biweek, sep="_"),
         transect_combo_day = paste(transect_combo, julian_date, sep="_"),
         )

spatiotemporal_scales <- c("location", "location_yr", "location_biweek", "location_day",
                           "combo", "combo_yr", "combo_biweek", "combo_day",
                           "transect_combo", "transect_combo_yr", "transect_combo_biweek", "transect_combo_day")


# Get average plant values ------------------
av_fun <- function(x){
  if (is.numeric(x)) {
    median(x, na.rm = TRUE)
  } else {
    names(which.max(table(x)))
  }
}



(plant_trait_cols <- colnames(plant_dat)[c(14:25, 31:34)])
(plant_quant_trait_cols <- colnames(plant_dat)[c(31:34)])

plant_trait_av <- plant_dat %>% 
  group_by(flower_species) %>% 
  dplyr::summarise(across(all_of(plant_trait_cols), av_fun),
                   plant_pheno_start = quantile(jdate, probs = 0.1) %>% as.numeric(),
                   plant_pheno_end = quantile(jdate, probs = 0.9) %>% as.numeric())
# Add a random column to gut check results
set.seed(1)
plant_trait_av$plant_rand <- rnorm(dim(plant_trait_av)[1])

plant_trait_quant_av <- plant_dat %>% 
  group_by(flower_species) %>% 
  dplyr::summarise(across(plant_trait_cols, av_fun))

set.seed(1)
plant_trait_quant_av$plant_rand <- rnorm(dim(plant_trait_quant_av)[1])


plant_pheno_cols <- c("plant_pheno_start", "plant_pheno_end")




# 
bee_dat <- bee_dat[order(bee_dat$julian_date), ]
bee_dat <- bee_dat[order(bee_dat$transect_combo), ]
#bee_dat %>% View()

# Phenology
filter(bee_dat, flower_species == "Allium cernuum")$jdate %>% as.numeric() %>% hist()
filter(bee_dat, genus_species == "Halictus_tripartitus")$jdate %>% as.numeric() %>% hist()


# Getting bee averages

(bee_trait_cols <- colnames(bee_dat)[c(9, 18, 20)])
(bee_quant_trait_cols <- colnames(bee_dat)[c(9)])

bee_trait_av <- bee_dat %>% 
  group_by(genus_species) %>% 
  dplyr::summarise(across(all_of(bee_trait_cols), av_fun),
                   bee_pheno_start = quantile(jdate, probs = 0.1) %>% as.numeric(),
                   bee_pheno_end = quantile(jdate, probs = 0.9) %>% as.numeric())
# Add a random column to gut check results
set.seed(1)
bee_trait_av$bee_rand <- rnorm(dim(bee_trait_av)[1])

bee_pheno_cols <- c("bee_pheno_start", "bee_pheno_end")

# Adding plant averages
bee_dat <- bee_dat %>% left_join(plant_trait_av)
#bee_dat %>% select(it_mm, plant_quant_trait_cols) %>% psych::pairs.panels()


# Going to change to factors
bee_dat <- bee_dat %>% mutate_if(is.character, as.factor)

# Make a list of columns to be used for prediction
trait_cols <- c("bin",
                bee_trait_cols,
                plant_trait_cols)
pheno_cols <- c("bin",
                bee_pheno_cols,
                plant_pheno_cols)
trait_pheno_cols <- c("bin",
                      bee_trait_cols,
                      plant_trait_cols,
                      bee_pheno_cols,
                      plant_pheno_cols)
sp_cols <- c("bin",
             bee_pheno_cols,
             plant_pheno_cols)

rand_cols <- c("bin",
             "bee_rand",
             "plant_rand")


# Here is a list to describe these 4 options
cols_list <- list(trait_cols, pheno_cols, 
                  trait_pheno_cols, sp_cols) #, rand_cols
names(cols_list) <- c("trait", "pheno", "trait_pheno", "sp") #, "rand"


gbm_results <- expand_grid(names(cols_list), 
                           spatiotemporal_scales)
colnames(gbm_results) <- c("preds", "scale")
gbm_results <- gbm_results %>% 
  add_column(accur = NA,
             null_accur = NA,
             kappa = NA,
             auc = NA)


for(i in spatiotemporal_scales){
  
  # Get info on what is and isn't observed
  dat_long <- tibble(genus_species = c(),
                     flower_species = c(),
                     count = c(),
                     spatiotemporal_scale = c())
  for(j in as.character(unlist(unique(bee_dat[,i])))){
    dat <- bee_dat[bee_dat[,i] == j,]
    dat <- table(list(as.character(dat$genus_species), as.character(dat$flower_species))) %>% as.data.frame()
    colnames(dat) <- c("genus_species", "flower_species", "count")
    
    dat$spatiotemporal_scale <- j
    
    dat_long <- rbind(dat_long, dat)
  }
  dim(dat_long)
  
  dat_long <- dat_long %>% mutate(bin = ifelse(count > 0, 1, 0))
  
  # Join trait data
  
  dat_long <- dat_long %>% 
    left_join(plant_trait_av) %>% 
    left_join(bee_trait_av)
  
  
  # Going to change to factors
  dat_long <- dat_long %>% mutate_if(is.character, as.factor)
  
  for(k in names(cols_list)){

    bee_mod <- gbm(formula = bin ~ .,
                   distribution = "bernoulli",
                   data = dat_long[,cols_list[[k]]],
                   interaction.depth = 4,
                   n.trees = 2000, # PROBABLY WANT THIS TO GO A BIT LONGER!!!!!
                   cv.folds = 5)
    
    gbm.perf(bee_mod, method = "cv")
    # par(mar = c(5, 15, 1, 1))
    # summary(
    #   bee_mod, 
    #   cBars = 10,
    #   method = relative.influence, # also can use permutation.test.gbm
    #   las = 2
    # )
    
    # bee_preds <- predict(object = bee_mod,
    #                      newdata = dat_long,
    #                      n.trees = gbm.perf(bee_mod, method = "cv", plot.it = F),
    #                      type = "response")
    # 
    # conf_mat <- confusionMatrix(factor(ifelse(bee_preds>0.5,1,0)), 
    #                             factor(dat_long$bin))
    
    conf_mat <- confusionMatrix(factor(ifelse(bee_mod$cv.fitted>0.5,1,0)),
                                factor(dat_long$bin))
    
    ind <- which(gbm_results$preds == k & gbm_results$scale == i)
    gbm_results$accur[ind] <- conf_mat$overall["Accuracy"]
    gbm_results$null_accur[ind] <- conf_mat$overall["AccuracyNull"]
    gbm_results$kappa[ind] <- conf_mat$overall["Kappa"]
    gbm_results$auc[ind] <- gbm.roc.area(dat_long$bin, bee_mod$cv.fitted)
    
    print(paste(i, k, Sys.time()))
    print(as.data.frame(gbm_results))
  }
  

  
} # Started at 7:41 pm 


old_gbm_results3 <- gbm_results

gbm_results <- gbm_results %>% 
  mutate(time_scale = rep(c("all", "year", "biweek", "day"), times = 12),
         spatial_scale = rep(rep(c("location", "combo", "transect"), each = 4),times = 4))



# Make a change in accuracy columns too
gbm_results <- gbm_results %>% 
  mutate(delta_accur = gtools::logit(accur) - gtools::logit(null_accur))

# Reorder
gbm_results$preds <- factor(gbm_results$preds, 
                            levels = c("sp", "pheno", "trait", "trait_pheno")) # , "rand"

gbm_results$scale <- factor(gbm_results$scale, 
                            levels = spatiotemporal_scales) #c("location", "combo", "transect_combo", "transect_combo_yr", "transect_combo_day"))

gbm_results$time_scale <- factor(gbm_results$time_scale, 
                                 levels = c("day", "biweek", "year", "all")) #c("location", "combo", "transect_combo", "transect_combo_yr", "transect_combo_day"))

gbm_results$spatial_scale <- factor(gbm_results$spatial_scale, 
                                 levels = c("location", "combo", "transect")) #c("location", "combo", "transect_combo", "transect_combo_yr", "transect_combo_day"))


# Make a plot
library("scales")
alpha <- 180
gbm_results %>% filter(preds != "rand") %>% 
  ggplot(aes(x=time_scale, y=auc, fill=preds)) + 
  #theme_minimal() +
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  scale_y_continuous(limits = c(0.65, 0.85), oob = rescale_none) +
  facet_wrap(~spatial_scale, ncol = 1,
             labeller = as_labeller(c(`location` = "Spatial scale: site (location)", 
                                      `combo` = "Spatial scale: plot within site (combo)", 
                                      `transect` = "Spatial scale: transect within plot (transect)"))) +
  labs(fill = "Predictor\nvariables:") +
  scale_fill_manual(labels = c("species identity", "phenology", "traits", "traits & phenology"),
                    values=c(rgb(255,127,0, 
                                 maxColorValue = 255,
                                 alpha = alpha), 
                             rgb(228,26,28, 
                                 maxColorValue = 255,
                                 alpha = alpha), 
                             rgb(55,126,184, 
                                 maxColorValue = 255,
                                 alpha = alpha),
                             rgb(152,78,163, 
                                 maxColorValue = 255,
                                 alpha = alpha)))

gbm_results %>% 
  group_by(scale) %>% 
  summarise(time_scale = mean(as.numeric(time_scale)),
            spatial_scale = mean(as.numeric(spatial_scale)),
            )

gbm_results %>% View()

summary_results <- expand_grid(levels(gbm_results$time_scale), 
                               levels(gbm_results$spatial_scale)) %>% 
  cbind(expand_grid(1:4, 1:3))

colnames(summary_results) <- c("time_scale", "spatial_scale",
                               "time", "space")
summary_results <- summary_results %>% 
  add_column(sp_minus_pheno = NA,
             traits_minus_pheno = NA,
             traits_pheno_minus_traits = NA,
             trait_contrib = NA)
result_cols <- c("sp_minus_pheno",
                 "traits_minus_pheno",
                 "traits_pheno_minus_traits",
                 "trait_contrib")
  
for(i in levels(gbm_results$time_scale)){
  for(j in levels(gbm_results$spatial_scale)){
    dat <- filter(gbm_results, time_scale == i & spatial_scale == j)
    
    sp <- dat$auc[dat$preds == "sp"]
    pheno <- dat$auc[dat$preds == "pheno"]
    trait <- dat$auc[dat$preds == "trait"]
    trait_pheno <- dat$auc[dat$preds == "trait_pheno"]
    
    ind <- which(summary_results$time_scale == i & summary_results$spatial_scale == j)
    summary_results[ind,result_cols] <- c(sp - pheno,
                                          trait - pheno,
                                          trait_pheno - trait,
                                          (trait_pheno - trait) / (trait_pheno - pheno))
    
  }
}


summary_results$time_scale <- factor(summary_results$time_scale, 
                                 levels = c("day", "biweek", "year", "all")) #c("location", "combo", "transect_combo", "transect_combo_yr", "transect_combo_day"))

summary_results$spatial_scale <- factor(summary_results$spatial_scale, 
                                    levels = c("location", "combo", "transect")) #c("location", "combo", "transect_combo", "transect_combo_yr", "transect_combo_day"))




png(filename = "how much is traits better than phenology.png", 
    width = 4,
    height = 3,
    units = "in",
    res = 300)

ggplot(data = summary_results, 
       aes(x=time_scale, y=traits_minus_pheno)) + 
  geom_point(aes(color = spatial_scale)) +
  geom_line(aes(x=time_scale, y=traits_minus_pheno, group = spatial_scale)) +
  theme_minimal() + 
  scale_color_manual(labels = c("site", "plot", "transect"),
                     values=c(rgb(255,127,0, 
                                  maxColorValue = 255,
                                  alpha = alpha), 
                              rgb(228,26,28, 
                                  maxColorValue = 255,
                                  alpha = alpha), 
                              rgb(55,126,184, 
                                  maxColorValue = 255,
                                  alpha = alpha))) +
  ylab("\u0394 AUC:\ntraits vs phenology")

dev.off()

png(filename = "how much does phenology help.png", 
    width = 4,
    height = 3,
    units = "in",
    res = 300)

ggplot(data = summary_results, 
       aes(x=time_scale, y=traits_pheno_minus_traits)) + 
  geom_point(aes(color = spatial_scale)) +
  geom_line(aes(x=time_scale, y=traits_pheno_minus_traits, group = spatial_scale)) +
  theme_minimal() + 
  scale_color_manual(labels = c("site", "plot", "transect"),
                     values=c(rgb(255,127,0, 
                                  maxColorValue = 255,
                                  alpha = alpha), 
                              rgb(228,26,28, 
                                  maxColorValue = 255,
                                  alpha = alpha), 
                              rgb(55,126,184, 
                                  maxColorValue = 255,
                                  alpha = alpha))) +
  ylab("\u0394 AUC:\ntraits & phenology vs traits alone")

dev.off()

  

as_labeller(c(`location` = "Spatial scale: site (location)", 
              `combo` = "Spatial scale: plot within site (combo)", 
              `transect` = "Spatial scale: transect within plot (transect)"))

plot(NA,
     xlim = c(0.5,5.5),
     ylim = c(0.5,1))

make_poly <- function(preds, col, metric){
  for(i in 1:length(preds)){
    #for(j in 1:length(scale)){
      dat <- gbm_results[as.character(gbm_results$preds) == preds[i],]
      
      xx <- c(1:5, 5, 1)
      yy <- c(unlist(dat[,metric]), 0, 0)
      
      polygon(xx, yy, col = col[i], border = F)
    #}
  }
}

make_poly(preds = names(cols_list)[3:1],
          #scale = spatiotemporal_scales,
          metric = "kappa",
          col = c("grey10", "grey30", "grey50"))

