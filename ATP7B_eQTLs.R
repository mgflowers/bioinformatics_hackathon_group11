####################################################################################
#	Script-file:   eQTLs data analysis                
#
#	Data used:	   ATP7B_eQTLs.xlsx
#
#	Plot created:  ATP7B_eQTLs.png
#
# Purpose:  	   Summarize data for gene ATP7B eQTLs and generate plot to show distribution
#
# Author:        Roger
# Date:          2024/12/08
####################################################################################

library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)


rm(list=ls())
# setwd("G:/OneDrive - Clemson University/Clemson documents/PhD Working Documents/合作项目组文件/")
setwd("C:/Users/Roger/Downloads")

eqtls <- read.csv("ATP7B_eQTLs.csv")
tissue_counts <- eqtls %>%
  group_by(Tissue) %>%
  summarise(count = n()) %>% 
  mutate(percentage = (count / sum(count)) * 100)


eqtls2 <- eqtls[eqtls$P_Value < 10^-10,]
tissue_counts2 <- eqtls2 %>%
  group_by(Tissue) %>%
  summarise(count = n()) %>% 
  mutate(percentage = (count / sum(count)) * 100)

wb <- createWorkbook()
addWorksheet(wb, "ATP7B_eQTLs")
writeData(wb, "ATP7B_eQTLs", eqtls)

addWorksheet(wb, "Tissue_variants_stats")
writeData(wb, "Tissue_variants_stats", tissue_counts)

addWorksheet(wb, "Tissue_variants_stats_selected")
writeData(wb, "Tissue_variants_stats_selected", tissue_counts2)

saveWorkbook(wb, "ATP7B_eQTLs_updated.xlsx", overwrite = TRUE)

p <- ggplot(tissue_counts2, aes(x = Tissue, y = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  theme_grey() +
  labs(title = "eQTLs for gene ATP7B (p < 1e-10)",
       x = "Tissues", y = "Variants Counts"
       ) + 
  theme( axis.text.x = element_text(angle = 55, hjust = 1),
         plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b=20)),
         axis.title.x = element_text(hjust = 0.5, size = 15),  
         axis.title.y = element_text(hjust = 0.5, size = 15, margin = margin(r=20))
  )
  
ggsave("ATP7B_eQTLs.png", plot = p, width = 10, height = 7)





