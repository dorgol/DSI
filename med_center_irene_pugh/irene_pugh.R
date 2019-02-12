# Med Center Irene Pugh

#She describes her project like this: the study was designed to test whether
#storing blood (for transfusion) at higher temperatures within the recommended
#temperature range results in a different metabolic activity over time. In the
#study, donated blood was followed over time (8 timepoints – column D) and was
#stored at one of three temperature groups (either 2º, 4º or 6º - column E).
#There were 72 metabolites measured for each sample (columns F and onward). So
#the effect of time and the effect of temperature are both of interest.*

# Questions: should I group by DONOR ID or BAG ID? (5 levels of former, 11 of latter) -- Doesn't seem to matter bc 11 levels when we interact with temparture either way

# 0. Setup ####

library(ggplot2)
library(readxl)
library(reshape2)
library(dplyr)
library(forcats)
library(plotly)
library(htmltools)
library(scales)
library(cowplot)
#devtools::install_github("rstudio/crosstalk")
library(crosstalk)

blood = read_xlsx("~/Documents/DSI/med_center_irene_pugh/Lipidomics_Data_BloodStorage-Temperature_02012019.xlsx")
#There are 11 bags (samples) measured at 9 timepoints
blood$`Storage Temperature` = c("2 °C", "4 °C", "6 °C")[as.factor(blood$`Storage Temperature (°C)`)]
blood = blood %>% select(-`Storage Temperature (°C)`)
blood$group = interaction(blood$"BAG ID", blood$"Storage Temperature")
blood.melt = melt(blood, id.vars = c("Sample ID", "BAG ID", "DONOR ID", "Timepoint", "Storage Temperature", "group"))
blood.melt = blood.melt %>% group_by(variable) %>% mutate(variance = var(value))
blood.melt$value = round(blood.melt$value, 3)

# 1. EDA ####

#  Static plots  ####
#    Plot all data without dimension reduction, in order of variance (so no-change varaibles are last) ----
metabolites_over_time = ggplot(blood.melt) +
  geom_line(aes(x = Timepoint, y = value, group = group, color = `Storage Temperature`), size = .3) +
  facet_wrap(.~fct_reorder(variable, -variance), scales = "free_y") + 
  scale_color_manual(values = hue_pal()(3)[3:1]) + 
  theme_bw() +
  theme(legend.position = "top", strip.background = element_blank(),
        strip.text = element_text(size = 7),
        axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6)) +
  ggtitle("All blood metabolite data over time")

ggsave("~/Documents/DSI/med_center_irene_pugh/metabolites_over_time.png", metabolites_over_time,
       height = 10, width = 16)

#        Linked version ####
shared_blood = SharedData$new(blood %>% group_by(group))

var_list = as.character(unique(blood.melt$variable)) 
# remove variables with zero variance. No point in plotting them.
var_remove = as.character((blood.melt %>% group_by(variable) %>% summarize(var = first(variance)) %>% filter(var == 0))[["variable"]])
var_list = var_list[! var_list %in% var_remove]

nrow1 = 11
ncol1 = ceiling(length(var_list)/nrow1)

plot_list = lapply(1:length(var_list), function(i) {
  showleg = ifelse(i==1, TRUE, FALSE)
  ylab = ifelse(i==1, "Value", "")
  xlab = ifelse(i==1, "Timepoint", "")
  shared_blood %>%
    plot_ly(y=~round(get(var_list[i], envir = as.environment(shared_blood$origData())), 3),
            x=~Timepoint,
            color = ~`Storage Temperature`,
            colors = hue_pal()(3)[3:1], #https://www.r-graph-gallery.com/121-manage-colors-with-plotly/
            hoverinfo = "text",
            height = 300
    ) %>%
    add_lines(name = ~`Storage Temperature`,
              hoverinfo = "none", 
              type = "scatter", mode = "lines") %>%
    add_markers(x = ~Timepoint,
                showlegend = FALSE, 
                text = paste("Sample ID:", blood$`Sample ID`,"\n",
                             "Value:", round(get(var_list[i], envir = as.environment(shared_blood$origData())), 3))) %>%
    layout(title = var_list[i],
           legend = list(x = 0.1, y = 0.95), #orientation = "h"
           yaxis = list(title = ylab), 
           xaxis = list(title = xlab),
           showlegend = showleg,
           margin = list(l = 20, r = 20, b = 20, t = 50, pad = 0)) %>%
    highlight(on = "plotly_selected", off = "plotly_deselect")
})

plot_list = split(plot_list, rep(1:ncol1, each=nrow1))

metabolites_over_time_interactive = bscols(
  plot_list[[1]],
  plot_list[[2]],
  plot_list[[3]],
  plot_list[[4]],
  plot_list[[5]],
  plot_list[[6]]
)

save_html(metabolites_over_time_interactive, file = "~/Documents/DSI/med_center_irene_pugh/metabolites_over_time.html")

#    Averaged so there's one line for each temp ----
metabolites_over_time_avg_by_tmp = ggplot(blood.melt %>% group_by(`Storage Temperature`, variable, Timepoint) %>% mutate(value = round(mean(value), 3))) +
  geom_line(aes(x = Timepoint, y = value, group = group, color = `Storage Temperature`), size = .5) +
  scale_color_manual(values = hue_pal()(3)[3:1]) +
  facet_wrap(.~fct_reorder(variable, -variance), scales = "free_y") + 
  theme_bw() +
  theme(legend.position = "top", strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6)) +
  ggtitle("All blood metabolite data over time, AVERAGED by storage temperature")

ggsave("~/Documents/DSI/med_center_irene_pugh/metabolites_over_time_avg_by_tmp.png",
       metabolites_over_time_avg_by_tmp, height = 10, width = 16)

#        Interactive version (No need for linked plot for average) ----

save_html(ggplotly(metabolites_over_time_avg_by_tmp, width = 1600, height = 1000) %>%
  add_lines(hoverinfo = "none") %>%
  layout(legend = list(x = 0, y = 1.1, orientation = "h")), 
  "~/Documents/DSI/med_center_irene_pugh/metabolites_over_time_avg_by_tmp.html")


#    Dimension reduction, in order of variance (so no-change varaibles are last) ----
blood.pcadata = blood %>% select( one_of( c(as.character(unique(blood.melt$variable[blood.melt$variance > 0])) ,
                                            "Storage Temperature", "Timepoint", "group") ))# remove variables with no variance

# About 70 pct of variance explained in first 4 components
pca.all = prcomp(blood.pcadata %>% 
                   select(-one_of("Storage Temperature", "Timepoint", "group")), center = TRUE, scale = TRUE)
summary(pca.all)$importance

blood.pca.results = cbind(blood.pcadata[,c("Storage Temperature", "Timepoint", "group")], pca.all$x[,1:4])
blood.pca.melt = melt(blood.pca.results, id.vars = c("Timepoint", "Storage Temperature", "group"))


#    Plot all data with composite-of-metabolites (dimension reduction), in order of variance (so no-change varaibles are last) ----
metabolite_pcvars = ggplot(blood.pca.melt) +
  geom_line(aes(x = Timepoint, y = value, group = group, color = `Storage Temperature`)) +
  scale_color_manual(values = hue_pal()(3)[3:1]) + 
  facet_wrap(.~variable, scales = "free_y") + 
  theme_bw() +
  theme(legend.position = "top", strip.background = element_blank(), strip.text = element_text(size = 7)) +
  ggtitle("Principal components of blood metabolite data over time")

metabolite_pcvars_avg_by_tmp = ggplot(blood.pca.melt %>% group_by(`Storage Temperature`, variable, Timepoint) %>% mutate(value = mean(value))) +
  geom_line(aes(x = Timepoint, y = value, group = group, color = `Storage Temperature`), size = .5) +
  scale_color_manual(values = hue_pal()(3)[3:1]) + 
  facet_wrap(.~variable, scales = "free_y") + 
  theme_bw() +
  theme(legend.position = "top", strip.background = element_blank(), strip.text = element_text(size = 7)) +
  ggtitle("Principal components of blood metabolite data over time, AVERAGED by storage temperature")

ggsave("~/Documents/DSI/med_center_irene_pugh/metabolite_pcvars.png", 
       plot_grid(metabolite_pcvars, metabolite_pcvars_avg_by_tmp, nrow = 1),
       height = 10, width = 16.5)


#    Plots showing elements of principal components ----

# First component
plot_pc1 = ggplot(data.frame(PC1 = round(pca.all$rotation[,1],3), variable = as.character(rownames(pca.all$rotation)))) +
  geom_bar(aes(y = PC1, x = fct_reorder(variable, PC1, .desc = TRUE)), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5)) +
  xlab("Metabolites ordered by loading in PC") +
  ylab("variable loading") + 
  ggtitle("PC1: 45% of variance explained")

# Second component
plot_pc2 = ggplot(data.frame(PC2 = round(pca.all$rotation[,2],3), variable = as.character(rownames(pca.all$rotation)))) +
  geom_bar(aes(y = PC2, x = fct_reorder(variable, PC2, .desc = TRUE)), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5)) +
  ylab("variable loading") + 
  xlab("Metabolites ordered by loading in PC") +
  ggtitle("PC2: 12% of variance explained")

# Third component
plot_pc3 = ggplot(data.frame(PC3 = round(pca.all$rotation[,3],3), variable = as.character(rownames(pca.all$rotation)))) +
  geom_bar(aes(y = PC3, x = fct_reorder(variable, PC3, .desc = TRUE)), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5)) +
  ylab("variable loading") + 
  xlab("Metabolites ordered by loading in PC") +
  ggtitle("PC3: 7% of variance explained")

# Fourth component
plot_pc4 = ggplot(data.frame(PC4 = round(pca.all$rotation[,4],3), variable = as.character(rownames(pca.all$rotation)))) +
  geom_bar(aes(y = PC4, x = fct_reorder(variable, PC4, .desc = TRUE)), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5)) +
  ylab("variable loading") + 
  ggtitle("PC4: 5% of variance explained") +
  xlab("Metabolites ordered by loading in PC")

metabolite_pcs = plot_grid(plot_pc1, plot_pc2, plot_pc3, plot_pc4)

ggsave("~/Documents/DSI/med_center_irene_pugh/metabolite_pcs.png", metabolite_pcs,
       height = 10, width = 16)

