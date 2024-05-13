###### Genotype × environment interaction and Stability analysis ##############

######################Stability analysis with metan ############################
############################## WAASB Model #####################################

library(metan)
library(ggplot2)
library(ggrepel)
library(writexl)
library(openxlsx)
options(max.print = 10000)

############################### data import ####################################

stabdata<-read.csv(file.choose(),)
attach(stabdata)
str(stabdata)
options(max.print = 10000)

######################### factors with unique levels ###########################

stabdata$ENV <- factor(stabdata$ENV, levels=unique(stabdata$ENV))
stabdata$GEN <- factor(stabdata$GEN, levels=unique(stabdata$GEN))
stabdata$REP <- factor(stabdata$REP, levels=unique(stabdata$REP))
str(stabdata)


###################### extract trait name from data file #######################

traitall <- colnames(stabdata)[sapply(stabdata, is.numeric)]
traitall

################### Data inspection and cleaning functions #####################

inspect(stabdata, threshold= 50, plot=FALSE) %>% rmarkdown::paged_table()


for (trait in traitall) {
  find_outliers(stabdata, var = all_of(trait), plots = TRUE)
}

remove_rows_na(stabdata)
replace_zero(stabdata)
find_text_in_num(stabdata, var = all_of(trait))

############################# data analysis ####################################
########################### descriptive stats ##################################

if (!file.exists("output")) {
  dir.create("output")
}

ds <- desc_stat(stabdata, stats="all", hist = TRUE, plot_theme = theme_metan())


write_xlsx(ds, file.path("output", "Descriptive.xlsx"))


############################# mean performances ################################
############################# mean of genotypes ################################

mg <- mean_by(stabdata, GEN) 
mg
View(mg)

############################# mean of environments #############################

me <- mean_by(stabdata, ENV)
me
View(me)

################################# two way mean #################################

dm <- mean_by(stabdata, GEN, ENV)
dm
View(dm)

############### mean performance of genotypes across environments ##############

mge <- stabdata %>% 
  group_by(ENV, GEN) %>%
  desc_stat(stats="mean")
mge
View(mge)

############### Exporting all mean performances computed above #################

write_xlsx(
  list(
    "Genmean" = mg,
    "Envmean" = me,
    "Genmeaninenv" = dm,
    "Genmeaninenv2" = mge
  ),
  file.path("output", "Mean Performance.xlsx")
)

############################ two-way table for all #############################

twgy_list <- list()

for (trait in traitall) {
  twgy_list[[trait]] <- make_mat(stabdata, GEN, ENV, val = trait)
}

result_twgy <- list()
for (trait in traitall) {
  twgy_result<- as.data.frame(twgy_list[[trait]])
  result_twgy[[trait]] <- twgy_result
}

twgy_wb <- createWorkbook()
for (trait in traitall) {
  addWorksheet(twgy_wb, sheetName = paste0(trait, ""))
  writeData(twgy_wb, sheet = trait, x = result_twgy[[trait]], startCol = 2, startRow = 1)
  writeData(twgy_wb, sheet = trait, x = rownames(twgy_list[[trait]]), startCol = 1, startRow = 2)
  writeData(twgy_wb, sheet = trait, x = c("GEN"), startCol = 1, startRow = 1)
}

saveWorkbook(twgy_wb, file.path("output", "TWmean.xlsx"), overwrite = TRUE)

################### plotting performance across environments ###################
#################### make performance for all traits in one ####################
################################## Heatmap #####################################

perfor_heat_list <- list()

for (trait in traitall) {
  perfor_heat_list[[trait]] <-
    ge_plot(
      stabdata,
      ENV,
      GEN,
      !!sym(trait),
      type = 1,
      values = FALSE,
      average = FALSE,
      text_col_pos = c("bottom"),
      text_row_pos = c("left"),
      width_bar = 1.5,
      heigth_bar = 20,
      xlab = "ENV",
      ylab = "GEN",
      plot_theme = theme_metan(),
      colour = TRUE
    ) + geom_tile(color = "transparent") + labs(title = paste0(trait, " performance across eight environments")) + theme(legend.title = element_text(), axis.text.x.bottom = element_text(angle = 0, hjust = .5)) + guides(fill = guide_colourbar(title = trait, barwidth = 1.5, barheight = 20))
  assign(paste0(trait, "_perfor_heat"), perfor_heat_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(perfor_heat_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "perfor_heat_plot"))) {
  dir.create(file.path("output", "perfor_heat_plot"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "perfor_heat_plot", paste0(trait, ".png")),
         plot = perfor_heat_list[[trait]], width = 20, height = 30,
         dpi = 600, units = "cm")
}

################################# Line plot ####################################

perfor_line_list <- list()

for (trait in traitall) {
  perfor_line_list[[trait]] <-
    ge_plot(
      stabdata,
      ENV,
      GEN,
      !!sym(trait),
      type = 1,
      values = FALSE,
      average = FALSE,
      text_col_pos = c("bottom"),
      text_row_pos = c("left"),
      width_bar = 1.5,
      heigth_bar = 20,
      xlab = "ENV",
      ylab = "GEN",
      plot_theme = theme_metan(),
      colour = TRUE
    ) + geom_tile(color = "transparent") + labs(title = paste0(trait, " performance across eight environments")) + theme(legend.title = element_text(), axis.text.x.bottom = element_text(angle = 0, hjust = .5)) + guides(fill = guide_colourbar(title = trait, barwidth = 1.5, barheight = 20))
  assign(paste0(trait, "_perfor_line"), perfor_line_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(perfor_line_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "perfor_line_plot"))) {
  dir.create(file.path("output", "perfor_line_plot"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "perfor_line_plot", paste0(trait, ".png")),
         plot = perfor_line_list[[trait]], width = 20, height = 30,
         dpi = 600, units = "cm")
}

########################## Genotype-environment winners ########################
#edit better argument as for some variables lower values are preferred and higher for others ####

traitall ### view your traits to decide for above condition ###

win <-
  ge_winners(
    stabdata,
    ENV,
    GEN,
    resp = everything(),
    better = c(h, h, h, h, h, h, h, h, l, l, l, h, h, h, h, h, h, h, l) 
  )
win
View(win)
ranks <-
  ge_winners(
    stabdata,
    ENV,
    GEN,
    resp = everything(),
    type = "ranks",
    better = c(h, h, h, h, h, h, h, h, l, l, l, h, h, h, h, h, h, h, l)
  )
ranks
View(ranks)

write_xlsx(list("winner" = win, "ranks" = ranks), file.path("output", "winner rank.xlsx"))

############################ ge or gge effects #################################
######################### combined for all ge effects ##########################
########################## ge effects to excel #################################

ge_list <- ge_effects(stabdata, ENV, GEN, resp = everything(), type = "ge")

result_ge_list <- list()
for (trait in traitall) {
  ge_list_result <- as.data.frame(ge_list[[trait]])
  result_ge_list[[trait]] <- ge_list_result
}

########################### save all in one excel  #############################

ge_list_wb <- createWorkbook()
for (trait in traitall) {
  addWorksheet(ge_list_wb, sheetName = paste0(trait, ""))
  writeData(ge_list_wb, sheet = trait, x = result_ge_list[[trait]])
}
saveWorkbook(ge_list_wb, file.path("output","ge_effects.xlsx"), overwrite = TRUE)

############################## ge effects plots ################################

ge_plots <- list()

for (trait in traitall) {
  ge_plots[[trait]] <- plot(ge_list) + aes(ENV, GEN) + theme(legend.title = element_text()) + guides(fill = guide_colourbar(title = paste0(trait, " ge effects"), barwidth = 1.5, barheight = 20))
}  ## also coord_flip() in place of aes

################# print all plots once #########################################

for (trait in traitall) {
  print(ge_plots[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "ge_plots"))) {
  dir.create(file.path("output", "ge_plots"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "ge_plots", paste0(trait, ".png")),
         plot = ge_plots[[trait]], width = 20, height = 30,
         dpi = 600, units = "cm")
}

########################## gge effects to excel ################################

gge_list <- ge_effects(stabdata, ENV, GEN, resp = everything(), type = "gge")

result_gge_list <- list()
for (trait in traitall) {
  gge_list_result <- as.data.frame(gge_list[[trait]])
  result_gge_list[[trait]] <- gge_list_result
}

########################### save all in one excel  #############################

gge_list_wb <- createWorkbook()
for (trait in traitall) {
  addWorksheet(gge_list_wb, sheetName = paste0(trait, ""))
  writeData(gge_list_wb, sheet = trait, x = result_gge_list[[trait]])
}
saveWorkbook(gge_list_wb, file.path("output","gge_effects.xlsx"), overwrite = TRUE)

############################## ge effects plots ################################

gge_plots <- list()

for (trait in traitall) {
  gge_plots[[trait]] <- plot(gge_list) + aes(ENV, GEN) + theme(legend.title = element_text()) + guides(fill = guide_colourbar(title = paste0(trait, " gge effects"), barwidth = 1.5, barheight = 20))
}  ## also coord_flip() in place of aes

################# print all plots once #########################################

for (trait in traitall) {
  print(gge_plots[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "gge_plots"))) {
  dir.create(file.path("output", "gge_plots"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "gge_plots", paste0(trait, ".png")),
         plot = gge_plots[[trait]], width = 20, height = 30,
         dpi = 600, units = "cm")
}

############################ fixed effect models ###############################
########################### Individual  and Joint anova ########################
######################### Individual anova for all traits ######################

aovind_list <- anova_ind(stabdata, env = ENV, gen = GEN, rep = REP, resp = everything())

result_aovind <- list()
for (trait in traitall) {
  ind_result<- as.data.frame(aovind_list[[trait]]$individual)
  result_aovind[[trait]] <- ind_result
}

aovind_wb <- createWorkbook()
for (trait in traitall) {
  addWorksheet(aovind_wb, sheetName = paste0(trait, ""))
  writeData(aovind_wb, sheet = trait, x = result_aovind[[trait]])
}
saveWorkbook(aovind_wb, file.path("output","indaovall.xlsx"), overwrite = TRUE)

################## Joint anova for all traits (ANOVA) ##########################

aovjoin_list <- anova_joint(stabdata, env = ENV, gen = GEN, rep = REP, resp = everything())

result_aovjoin <- list()
for (trait in traitall) {
  join_result<- as.data.frame(aovjoin_list[[trait]]$anova)
  result_aovjoin[[trait]] <- join_result
}

aovjoin_wb <- createWorkbook()
for (trait in traitall) {
  addWorksheet(aovjoin_wb, sheetName = paste0(trait, ""))
  writeData(aovjoin_wb, sheet = trait, x = result_aovjoin[[trait]])
}
saveWorkbook(aovjoin_wb, file.path("output","joinaovall.xlsx"), overwrite = TRUE)

################## Joint anova for all traits (Details) (2) ####################

result_aovjoin2 <- list()
for (trait in traitall) {
  join_result2<- as.data.frame(aovjoin_list[[trait]]$details)
  result_aovjoin2[[trait]] <- join_result2
}

aovjoin_wb2 <- createWorkbook()
for (trait in traitall) {
  addWorksheet(aovjoin_wb2, sheetName = paste0(trait, ""))
  writeData(aovjoin_wb2, sheet = trait, x = result_aovjoin2[[trait]])
}
saveWorkbook(aovjoin_wb2, file.path("output", "joinaovall2.xlsx"), overwrite = TRUE)

############# WAASB based stability analysis for all (BLUPgen) #################

waasb_list <-
  waasb(
    stabdata,
    ENV,
    GEN,
    REP,
    resp = everything(),
    prob = 0.05,
    random = "gen", #change to 'env', 'all'
    mresp = c("h, h, h, h, h, h, h, h, l, l, l, h, h, h, h, h, h, h, l"),
    #wresp = c(50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50) variables in resp have equal weights for mean performance and stability (i.e., wresp = 50). chanage if equired
  )

result_waasb <- list()
for (trait in traitall) {
  waasb_result<- as.data.frame(waasb_list[[trait]]$BLUPgen)
  result_waasb[[trait]] <- waasb_result
}

waasb_wb <- createWorkbook()
for (trait in traitall) {
  addWorksheet(waasb_wb, sheetName = paste0(trait, ""))
  writeData(waasb_wb, sheet = trait, x = result_waasb[[trait]])
}
saveWorkbook(waasb_wb, file.path("output","waasbBLUPgen.xlsx"), overwrite = TRUE)

############# WAASB based stability analysis for all (BLUPint) #################

result_waasb2 <- list()
for (trait in traitall) {
  waasb_result2<- as.data.frame(waasb_list[[trait]]$BLUPint)
  result_waasb2[[trait]] <- waasb_result2
}

waasb_wb2 <- createWorkbook()
for (trait in traitall) {
  addWorksheet(waasb_wb2, sheetName = paste0(trait, ""))
  writeData(waasb_wb2, sheet = trait, x = result_waasb2[[trait]])
}
saveWorkbook(waasb_wb2, file.path("output", "waasbBLUPint.xlsx"), overwrite = TRUE)

############## WAASB based stability analysis for all (model) ##################

result_waasb3 <- list()
for (trait in traitall) {
  waasb_result3<- as.data.frame(waasb_list[[trait]]$model)
  result_waasb3[[trait]] <- waasb_result3
}

waasb_wb3 <- createWorkbook()
for (trait in traitall) {
  addWorksheet(waasb_wb3, sheetName = paste0(trait, ""))
  writeData(waasb_wb3, sheet = trait, x = result_waasb3[[trait]])
}
saveWorkbook(waasb_wb3, file.path("output", "waasbmodel.xlsx"), overwrite = TRUE)

############# WAASB based stability analysis for all (Means G*E) ###############

result_waasb4 <- list()
for (trait in traitall) {
  waasb_result4<- as.data.frame(waasb_list[[trait]]$MeansGxE)
  result_waasb4[[trait]] <- waasb_result4
}

waasb_wb4 <- createWorkbook()
for (trait in traitall) {
  addWorksheet(waasb_wb4, sheetName = paste0(trait, ""))
  writeData(waasb_wb4, sheet = trait, x = result_waasb4[[trait]])
}
saveWorkbook(waasb_wb4, file.path("output","waasbmeansge.xlsx"), overwrite = TRUE)

######################### Biplots for all in one AMMI 1 ########################

waasb1_list <- list()

for (trait in traitall) {
  waasb1_list[[trait]] <- plot_scores(waasb_list,
                                     var = trait,
                                     type = 1,
                                     first = "PC1",
                                     second = "PC2",
                                     x.lab = trait,
                                     repel = TRUE,
                                     max_overlaps = 50,
                                     shape.gen = 21,
                                     shape.env = 23,
                                     size.shape.gen = 2,
                                     size.shape.env = 3,
                                     col.bor.gen = "#215C29",
                                     col.bor.env = "#F68A31",
                                     col.line = "grey",
                                     col.gen = "#215C29",
                                     col.env = "#F68A31",
                                     col.segm.gen = transparent_color(),
                                     col.segm.env = "#F68A31",
                                     size.tex.gen = 3,
                                     size.tex.env = 3,
                                     size.tex.lab = 12,
                                     size.line = .4,
                                     line.type = 'dotdash',
                                     line.alpha = .8,
                                     highlight =,
                                     plot_theme = theme_metan(),
                                     size.segm.line = .4,
                                     leg.lab = c("Environment", "Genotype")) + labs(title = paste0("AMMI 1 biplot for ", trait)) + theme(
                                       plot.title = element_text(color = "black"),
                                       panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"),
                                       legend.position = c(0.88, .05),
                                       legend.background = element_rect(fill = NA)
                                     )
  assign(paste0(trait, "_waasb1"), waasb1_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(waasb1_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "waasb1_plots"))) {
  dir.create(file.path("output", "waasb1_plots"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output","waasb1_plots", paste0(trait, ".png")),
         plot = waasb1_list[[trait]], width = 15, height = 15,
         dpi = 600, units = "cm")
}

######################## Biplots for all in one AMMI 2 ######################### 

waasb2_list <- list()

for (trait in traitall) {
  waasb2_list[[trait]] <- plot_scores(waasb_list,
                                     var = trait,
                                     type = 2,
                                     first = "PC1",
                                     second = "PC2",
                                     repel = TRUE,
                                     max_overlaps = 50,
                                     shape.gen = 21,
                                     shape.env = 23,
                                     size.shape.gen = 2,
                                     size.shape.env = 3,
                                     col.bor.gen = "#215C29",
                                     col.bor.env = "#F68A31",
                                     col.line = "grey",
                                     col.gen = "#215C29",
                                     col.env = "#F68A31",
                                     col.segm.gen = transparent_color(),
                                     col.segm.env = "#F68A31",
                                     size.tex.gen = 3,
                                     size.tex.env = 3,
                                     size.tex.lab = 12,
                                     size.line = .4,
                                     line.type = 'dotdash',
                                     line.alpha = .8,
                                     highlight =,
                                     plot_theme = theme_metan(),
                                     size.segm.line = .4,
                                     leg.lab = c("Environment", "Genotype")) + labs(title = paste0("AMMI 2 biplot for ", trait)) + theme(
                                       plot.title = element_text(color = "black"),
                                       panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"),
                                       legend.position = c(0.88, .05),
                                       legend.background = element_rect(fill = NA))
  assign(paste0(trait, "_waasb2"), waasb2_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(waasb2_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "waasb2_plots"))) {
  dir.create(file.path("output", "waasb2_plots"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "waasb2_plots", paste0(trait, ".png")),
         plot = waasb2_list[[trait]], width = 15, height = 15,
         dpi = 600, units = "cm")
}

######################### Waasb biplots for all in one  ########################

waasb3_list <- list()

for (trait in traitall) {
  waasb3_list[[trait]] <- plot_scores(waasb_list,
                                     var = trait,
                                     type = 3,
                                     first = "PC1",
                                     second = "PC2",
                                     x.lab = trait,
                                     repel = TRUE,
                                     max_overlaps = 50,
                                     shape.gen = 21,
                                     shape.env = 23,
                                     size.shape.gen = 2,
                                     size.shape.env = 3,
                                     col.bor.gen = "#215C29",
                                     col.bor.env = "#F68A31",
                                     col.line = "grey",
                                     col.gen = "#215C29",
                                     col.env = "#F68A31",
                                     col.segm.gen = transparent_color(),
                                     col.segm.env = "#F68A31",
                                     size.tex.gen = 3,
                                     size.tex.env = 3,
                                     size.tex.lab = 12,
                                     size.line = .4,
                                     line.type = 'dotdash',
                                     line.alpha = .8,
                                     highlight =,
                                     plot_theme = theme_metan(),
                                     size.segm.line = .4,
                                     leg.lab = c("Environment", "Genotype")) + labs(title = paste0("WAASB biplot for ", trait)) + theme(
                                       plot.title = element_text(color = "black"),
                                       panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"),
                                       legend.position = c(0.88, .05),
                                       legend.background = element_rect(fill = NA))
  assign(paste0(trait, "_waasb3"), waasb3_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(waasb3_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "waasb3_plots"))) {
  dir.create(file.path("output", "waasb3_plots"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "waasb3_plots", paste0(trait, ".png")),
         plot = waasb3_list[[trait]], width = 15, height = 15,
         dpi = 600, units = "cm")
}

##################### WAASB scores plot for various traits ##################### 

waasb_score_list <- list()

for (trait in traitall) {
  waasb_score_list[[trait]] <- plot_waasby(waasb_list,
                                     var = trait,
                                     size.shape = 3.5,
                                     size.tex.lab = 12,
                                     col.shape = c("#215C29", "#F68A31"),
                                     x.lab = "WAASBY",
                                     y.lab = "Genotypes",
                                     x.breaks = waiver(),
                                     plot_theme = theme_metan()) + labs(title = paste0("WAASBY score for ", trait)) + theme(
                                       plot.title = element_text(color = "black"),
                                       panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"),
                                       legend.position = c(0.88, .05),
                                       legend.background = element_rect(fill = NA))
  assign(paste0(trait, "_waasb_score"), waasb_score_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(waasb_score_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "waasb_score_plots"))) {
  dir.create(file.path("output", "waasb_score_plots"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "waasb_score_plots", paste0(trait, ".png")),
         plot = waasb_score_list[[trait]], width = 20, height = 30,
         dpi = 600, units = "cm")
}

############## Weighting between stability and mean performance ################

scenarios<- wsmp(waasb_list, increment = 10)

############################### type 1 (based on PCA) ##########################

scenarios_plot1_list <- list()

for (trait in traitall) {
  scenarios_plot1_list[[trait]] <- plot(scenarios, var = trait, type = 1)
  assign(paste0(trait, "_scenario_plot1"), scenarios_plot1_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(scenarios_plot1_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "scenario_plots1"))) {
  dir.create(file.path("output", "scenario_plots1"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "scenario_plots1", paste0(trait, ".png")),
         plot = scenarios_plot1_list[[trait]], width = 20, height = 20,
         dpi = 600, units = "cm")
}

################## type 2 (based on mean vs stability) #########################

scenarios_plot2_list <- list()

for (trait in traitall) {
  scenarios_plot2_list[[trait]] <- plot(scenarios, var = trait, type = 2)
  assign(paste0(trait, "_scenario_plot2"), scenarios_plot2_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(scenarios_plot2_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "scenario_plots2"))) {
  dir.create(file.path("output", "scenario_plots2"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "scenario_plots2", paste0(trait, ".png")),
         plot = scenarios_plot2_list[[trait]], width = 20, height = 20,
         dpi = 600, units = "cm")
}

################## BLUP plot for various traits (Genotypes) ####################

blup_gen_list <- list()

for (trait in traitall) {
  blup_gen_list[[trait]] <- plot_blup(waasb_list,
                                           var = trait,
                                           which = "gen",
                                           prob = 0.05,
                                           size.shape = 3.5,
                                           size.tex.lab = 12,
                                           err.bar = TRUE,
                                           size.err.bar = 0.5,
                                           height.err.bar = 0.3,
                                           col.shape = c("#215C29", "#F68A31"),
                                           x.lab = paste0("Predicted ", trait),
                                           y.lab = "Genotypes",
                                           x.breaks = waiver(),
                                           plot_theme = theme_metan()) + labs(title = paste0("Predicted BLUPs for ", trait)) + theme(
                                             plot.title = element_text(color = "black"),
                                             panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"),
                                             legend.position = c(0.88, .05),
                                             legend.background = element_rect(fill = NA))
  assign(paste0(trait, "_blup_gen"), blup_gen_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(blup_gen_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "blup_gen_plots"))) {
  dir.create(file.path("output", "blup_gen_plots"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "blup_gen_plots", paste0(trait, ".png")),
         plot = blup_gen_list[[trait]], width = 20, height = 30,
         dpi = 600, units = "cm")
}

########## BLUP plot for various traits (Genotypes - Environment) ##############

blup_ge_list <- list()

for (trait in traitall) {
  blup_ge_list[[trait]] <- plot_blup(waasb_list,
                                      var = trait,
                                      which = "ge",
                                      prob = 0.05,
                                      ncol = 4,
                                      nrow = 2,
                                      size.shape = 2.5,
                                      size.tex.lab = 8,
                                      err.bar = TRUE,
                                      size.err.bar = 0.5,
                                      height.err.bar = 0.3,
                                      col.shape = c("#215C29", "#F68A31"),
                                      x.lab = paste0("Predicted ", trait),
                                      y.lab = "Genotypes",
                                      x.breaks = waiver(),
                                      plot_theme = theme_metan()) + labs(title = paste0("Predicted BLUPs for ", trait, " across environments")) + theme(
                                        plot.title = element_text(color = "black"),
                                        panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"),
                                        legend.position = c(0.95, .05),
                                        legend.background = element_rect(fill = NA))
  assign(paste0(trait, "_blup_ge"), blup_ge_list[[trait]])
}

############################## print all plots once ############################

for (trait in traitall) {
  print(blup_ge_list[[trait]])
}

##################### high quality save all in one  ############################

if (!file.exists("output")) {
  dir.create("output")
}

if (!file.exists(file.path("output", "blup_ge_plots"))) {
  dir.create(file.path("output", "blup_ge_plots"))
}

for (trait in traitall) {
  ggsave(filename = file.path("output", "blup_ge_plots", paste0(trait, ".png")),
         plot = blup_ge_list[[trait]], width = 30, height = 40,
         dpi = 600, units = "cm")
}

################################## blup indexes ################################

blup_indexes <- blup_indexes(waasb_list)

result_blup_indexes <- list()
for (trait in traitall) {
  blup_indexes_result<- as.data.frame(blup_indexes[[trait]])
  result_blup_indexes[[trait]] <- blup_indexes_result
}

blup_indexes_wb <- createWorkbook()
for (trait in traitall) {
  addWorksheet(blup_indexes_wb, sheetName = paste0(trait, ""))
  writeData(blup_indexes_wb, sheet = trait, x = result_blup_indexes[[trait]])
}
saveWorkbook(blup_indexes_wb, file.path("output","BLUPindexes.xlsx"), overwrite = TRUE)

######################## FAI-BLUP selection index ##############################

fai_selection <- fai_blup(waasb_list, verbose = TRUE)

plot(fai_selection)



