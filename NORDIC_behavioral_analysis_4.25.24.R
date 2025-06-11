# Import libraries & read in file--------------------------------------------------------
# Import libraries
library(tidyverse)
library(ggpubr)
library(readxl)

# Read in file
nordic_mastersheet <- read_xlsx("/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/NORDIC/NORDIC_behavioral_master_4.22.24.xlsx")
print(nordic_mastersheet)

# Clean data --------------------------------------------------------------
# Split by subgroup
## Controls
nordic_controls <- nordic_mastersheet[which(nordic_mastersheet$Subgroup == "CN"), ]
print(nordic_controls)

## PPA
nordic_ppa <- nordic_mastersheet %>%
  filter(Subgroup != "CN")
print(nordic_ppa)

# Split by processed group
## Orig
controls_orig <- nordic_controls[which(nordic_controls$Processed_group == "Orig"), ]
print(controls_orig)

ppa_orig <- nordic_ppa[which(nordic_ppa$Processed_group == "Orig"), ]
print(ppa_orig)

# Split by hemisphere
ppa_orig_left <- ppa_orig[which(ppa_orig$Hemisphere == "left"), ]
print(ppa_orig_left)

ppa_orig_right <- ppa_orig[which(ppa_orig$Hemisphere == "right"), ]
print(ppa_orig_right)

## Nordic
controls_nordic <- nordic_controls[which(nordic_controls$Processed_group == "NORDIC"), ]
print(controls_nordic)

ppa_nordic <- nordic_ppa[which(nordic_ppa$Processed_group == "NORDIC"), ] 
print(ppa_nordic)

# Split by hemisphere
ppa_nordic_left <- ppa_nordic[which(ppa_nordic$Hemisphere == "left"), ]
print(ppa_nordic_left)

ppa_nordic_right <- ppa_nordic[which(ppa_nordic$Hemisphere == "right"), ]
print(ppa_nordic_right)

# Correlation analysis for controls (IFG-MTG) ------------------------------------------
# Run correlations for IFG_MTG connectivity
## Orig IFG-MTG & BNT
# Calculate correlation by group
correlation_by_group <- nordic_mastersheet %>%
  group_by(Processed_group) %>%
  summarise(correlation = cor(IFG_MTG, BNT_100, 
                                   method = "pearson"))

# View the result
print(correlation_by_group)

controls_orig_IFG_MTG_BNT_corr <- cor.test(controls_orig$IFG_MTG, controls_orig$BNT_100, 
                                           method = "pearson")
print(controls_orig_IFG_MTG_BNT_corr)
ggscatter(controls_orig, x = "IFG_MTG", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity", ylab = "BNT")

## Orig IFG_MTG & PPVT
controls_orig_IFG_MTG_PPVT_corr <- cor.test(controls_orig$IFG_MTG, controls_orig$PPVT_100, 
                                           method = "pearson")
print(controls_orig_IFG_MTG_PPVT_corr)
ggscatter(controls_orig, x = "IFG_MTG", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity", ylab = "PPVT")

## Orig IFG_MTG & Grammar
controls_orig_IFG_MTG_grammar_corr <- cor.test(controls_orig$IFG_MTG, controls_orig$Grammar_Total_100,
                                              method = "pearson")
print(controls_orig_IFG_MTG_grammar_corr)
ggscatter(controls_orig, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity", ylab = "Grammar")

## Orig IFG_MTG & Repetition
controls_orig_IFG_MTG_repetition_corr <- cor.test(controls_orig$IFG_MTG, controls_orig$Repetition_100,
                                                  method = "pearson")
print(controls_orig_IFG_MTG_repetition_corr)
ggscatter(controls_orig, x = "IFG_MTG", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity", ylab = "Repetition")

## Nordic IFG_MTG & BNT
controls_nordic_IFG_MTG_BNT_corr <- cor.test(controls_nordic$IFG_MTG, controls_nordic$BNT_100,
                                              method = "pearson")
print(controls_nordic_IFG_MTG_BNT_corr)
ggscatter(controls_nordic, x = "IFG_MTG", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity ", ylab = "BNT",
          title = "Correlation of Control Left Hemisphere IFG-MTG Connectivity & BNT for NORDIC-processed Data") +
  theme_classic()

## Nordic (IFG_MTG & PPVT)
controls_nordic_IFG_MTG_PPVT_corr <- cor.test(controls_nordic$IFG_MTG, controls_nordic$PPVT_100,
                                               method = "pearson")
print(controls_nordic_IFG_MTG_PPVT_corr)
ggscatter(controls_nordic, x = "IFG_MTG", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "PPVT")

## LH nordic (IFG_MTG & Grammar)
lh_controls_nordic_IFG_MTG_grammar_corr <- cor.test(lh_controls_nordic$IFG_MTG, lh_controls_nordic$Grammar_Total_100,
                                                  method = "pearson")
print(lh_controls_nordic_IFG_MTG_grammar_corr)
ggscatter(lh_controls_nordic, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "Grammar")

## LH nordic (IFG_MTG & Repetition)
lh_controls_nordic_IFG_MTG_repetition_corr <- cor.test(lh_controls_nordic$IFG_MTG, lh_controls_nordic$Repetition_100,
                                                     method = "pearson")
print(lh_controls_nordic_IFG_MTG_repetition_corr)
ggscatter(lh_controls_nordic, x = "IFG_MTG", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "Repetition")

## RH nordic (IFG_MTG & BNT)
rh_controls_nordic_IFG_MTG_BNT_corr <- cor.test(rh_controls_nordic$IFG_MTG, rh_controls_nordic$BNT_100,
                                                method = "pearson")
print(rh_controls_nordic_IFG_MTG_BNT_corr)
ggscatter(rh_controls_nordic, x = "IFG_MTG", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "BNT",
          title = "Correlation of Control Right Hemisphere IFG-MTG Connectivity & BNT for NORDIC-processed Data") +
  theme_classic()

## RH nordic (IFG_MTG & PPVT)
rh_controls_nordic_IFG_MTG_PPVT_corr <- cor.test(rh_controls_nordic$IFG_MTG, rh_controls_nordic$PPVT_100,
                                                 method = "pearson")
print(rh_controls_nordic_IFG_MTG_PPVT_corr)
ggscatter(rh_controls_nordic, x = "IFG_MTG", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "PPVT")

## RH nordic (IFG_MTG & Grammar)
rh_controls_nordic_IFG_MTG_grammar_corr <- cor.test(rh_controls_nordic$IFG_MTG, rh_controls_nordic$Grammar_Total_100,
                                                    method = "pearson")
print(rh_controls_nordic_IFG_MTG_grammar_corr)
ggscatter(rh_controls_nordic, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "Grammar")

## RH nordic (IFG_MTG & Repetition)
rh_controls_nordic_IFG_MTG_repetition_corr <- cor.test(rh_controls_nordic$IFG_MTG, rh_controls_nordic$Repetition_100,
                                                       method = "pearson")
print(rh_controls_nordic_IFG_MTG_repetition_corr)
ggscatter(rh_controls_nordic, x = "IFG_MTG", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "Repetition")

# Correlation analysis for controls (IFG-ATL) ------------------------------------------
# Run correlations for IFG_ATL connectivity
## LH orig (IFG_ATL & BNT)
lh_controls_orig_IFG_ATL_BNT_corr <- cor.test(lh_controls_orig$IFG_ATL, lh_controls_orig$BNT_100,
                                              method = "pearson")
print(lh_controls_orig_IFG_ATL_BNT_corr)
ggscatter(lh_controls_orig, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "BNT")

## LH orig (IFG_ATL & PPVT)
lh_controls_orig_IFG_ATL_PPVT_corr <- cor.test(lh_controls_orig$IFG_ATL, lh_controls_orig$PPVT_100,
                                               method = "pearson")
print(lh_controls_orig_IFG_ATL_PPVT_corr)
ggscatter(lh_controls_orig, x = "IFG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "PPVT")

## LH orig (IFG_ATL & Grammar)
lh_controls_orig_IFG_ALT_grammar_corr <- cor.test(lh_controls_orig$IFG_ATL, lh_controls_orig$Grammar_Total_100,
                                                  method = "pearson")
print(lh_controls_orig_IFG_ALT_grammar_corr)
ggscatter(lh_controls_orig, x = "IFG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "Grammar")

## LH orig (IFG_ATL & Repetition)
lh_controls_orig_IFG_ALT_repetition_corr <- cor.test(lh_controls_orig$IFG_ATL, lh_controls_orig$Repetition_100,
                                                     method = "pearson")
print(lh_controls_orig_IFG_ALT_repetition_corr)
ggscatter(lh_controls_orig, x = "IFG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "Repetition")

## RH orig (IFG_ATL & BNT)
rh_controls_orig_IFG_ATL_BNT_corr <- cor.test(rh_controls_orig$IFG_ATL, rh_controls_orig$BNT_100,
                                              method = "pearson")
print(rh_controls_orig_IFG_ATL_BNT_corr)
ggscatter(rh_controls_orig, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH orig (IFG_ATL & PPVT)
rh_controls_orig_IFG_ATL_PPVT_corr <- cor.test(rh_controls_orig$IFG_ATL, rh_controls_orig$PPVT_100,
                                               method = "pearson")
print(rh_controls_orig_IFG_ATL_PPVT_corr)
ggscatter(rh_controls_orig, x = "IFG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "PPVT")

## RH orig (IFG_ATL & Grammar)
rh_controls_orig_IFG_ATL_grammar_corr <- cor.test(rh_controls_orig$IFG_ATL, rh_controls_orig$Grammar_Total_100,
                                                  method = "pearson")
print(rh_controls_orig_IFG_ATL_grammar_corr)
ggscatter(rh_controls_orig, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "Grammar")

## RH orig (IFG_ATL & Repetition)
rh_controls_orig_IFG_ATL_repetition_corr <- cor.test(rh_controls_orig$IFG_ATL, rh_controls_orig$Repetition_100,
                                                     method = "pearson")
print(rh_controls_orig_IFG_ATL_repetition_corr)
ggscatter(rh_controls_orig, x = "IFG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "Repetition")

## LH nordic (IFG_ATL & BNT)
lh_controls_nordic_IFG_ATL_BNT_corr <- cor.test(lh_controls_nordic$IFG_ATL, lh_controls_nordic$BNT_100,
                                                method = "pearson")
print(lh_controls_nordic_IFG_ATL_BNT_corr)
ggscatter(lh_controls_nordic, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "BNT")

## LH nordic (IFG_ATL & PPVT)
lh_controls_nordic_IFG_ATL_PPVT_corr <- cor.test(lh_controls_nordic$IFG_ATL, lh_controls_nordic$PPVT_100,
                                                 method = "pearson")
print(lh_controls_nordic_IFG_ATL_PPVT_corr)
ggscatter(lh_controls_nordic, x = "IFG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "PPVT")

## LH nordic (IFG_ATL & Grammar)
lh_controls_nordic_IFG_ATL_grammar_corr <- cor.test(lh_controls_nordic$IFG_ATL, lh_controls_nordic$Grammar_Total_100,
                                                    method = "pearson")
print(lh_controls_nordic_IFG_ATL_grammar_corr)
ggscatter(lh_controls_nordic, x = "IFG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "Grammar")

## LH nordic (IFG_ATL & Repetition)
lh_controls_nordic_IFG_ATL_repetition_corr <- cor.test(lh_controls_nordic$IFG_ATL, lh_controls_nordic$Repetition_100,
                                                       method = "pearson")
print(lh_controls_nordic_IFG_ATL_repetition_corr)
ggscatter(lh_controls_nordic, x = "IFG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "Repetition")

## RH nordic (IFG_ATL & BNT)
rh_controls_nordic_IFG_ATL_BNT_corr <- cor.test(rh_controls_nordic$IFG_ATL, rh_controls_nordic$BNT_100,
                                                method = "pearson")
print(rh_controls_nordic_IFG_ATL_BNT_corr)
ggscatter(rh_controls_nordic, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH nordic (IFG_ATL & PPVT)
rh_controls_nordic_IFG_ATL_PPVT_corr <- cor.test(rh_controls_nordic$IFG_ATL, rh_controls_nordic$PPVT_100,
                                                 method = "pearson")
print(rh_controls_nordic_IFG_ATL_PPVT_corr)
ggscatter(rh_controls_nordic, x = "IFG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "PPVT")

## RH nordic (IFG_ATL & Grammar)
rh_controls_nordic_IFG_ATL_grammar_corr <- cor.test(rh_controls_nordic$IFG_ATL, rh_controls_nordic$Grammar_Total_100,
                                                    method = "pearson")
print(rh_controls_nordic_IFG_ATL_grammar_corr)
ggscatter(rh_controls_nordic, x = "IFG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "Grammar",
          title = "Correlation of Control Right Hemisphere IFG-ATL Connectivity & Grammar for NORDIC-processed Data",
          lable.rectangle = TRUE) +
  theme_classic()

## RH nordic (IFG_ATL & Repetition)
rh_controls_nordic_IFG_ATL_repetition_corr <- cor.test(rh_controls_nordic$IFG_ATL, rh_controls_nordic$Repetition_100,
                                                       method = "pearson")
print(rh_controls_nordic_IFG_ATL_repetition_corr)
ggscatter(rh_controls_nordic, x = "IFG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "Repetition")

# Correlation analysis for controls (MTG-ATL) ------------------------------------------
# Run correlations for MTG_ATL connectivity
## LH orig (MTG_ATL & BNT)
lh_controls_orig_MTG_ATL_BNT_corr <- cor.test(lh_controls_orig$MTG_ATL, lh_controls_orig$BNT_100,
                                              method = "pearson")
print(lh_controls_orig_MTG_ATL_BNT_corr)
ggscatter(lh_controls_orig, x = "MTG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "BNT")

## LH orig (MTG_ATL & PPVT)
lh_controls_orig_MTG_ATL_PPVT_corr <- cor.test(lh_controls_orig$MTG_ATL, lh_controls_orig$PPVT_100,
                                               method = "pearson")
print(lh_controls_orig_MTG_ATL_PPVT_corr)
ggscatter(lh_controls_orig, x = "MTG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "PPVT")

## LH orig (MTG_ATL & Grammar)
lh_controls_orig_MTG_ATL_grammar_corr <- cor.test(lh_controls_orig$MTG_ATL, lh_controls_orig$Grammar_Total_100,
                                                  method = "pearson")
print(lh_controls_orig_MTG_ATL_grammar_corr)
ggscatter(lh_controls_orig, x = "MTG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "Grammar")

## LH orig (MTG_ATL & Repetition)
lh_controls_orig_MTG_ATL_repetition_corr <- cor.test(lh_controls_orig$MTG_ATL, lh_controls_orig$Repetition_100,
                                                     method = "pearson")
print(lh_controls_orig_MTG_ATL_repetition_corr)
ggscatter(lh_controls_orig, x = "MTG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "Repetition",
          title = "Correlation of Control Left Hemisphere MTG-ATL Connectivity & Grammar for Original Data") +
  theme_classic()

## RH orig (MTG_ATL & BNT)
rh_controls_orig_MTG_ATL_BNT_corr <- cor.test(rh_controls_orig$MTG_ATL, rh_controls_orig$BNT_100,
                                              method = "pearson")
print(rh_controls_orig_MTG_ATL_BNT_corr)
ggscatter(rh_controls_orig, x = "MTG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH orig (MTG_ATL & PPVT)
rh_controls_orig_MTG_ATL_PPVT_corr <- cor.test(rh_controls_orig$MTG_ATL, rh_controls_orig$PPVT_100,
                                               method = "pearson")
print(rh_controls_orig_MTG_ATL_PPVT_corr)
ggscatter(rh_controls_orig, x = "MTG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "PPVT")

## RH orig (MTG_ATL & Grammar)
rh_controls_orig_MTG_ATL_grammar_corr <- cor.test(rh_controls_orig$MTG_ATL, rh_controls_orig$Grammar_Total_100,
                                                  method = "pearson")
print(rh_controls_orig_MTG_ATL_grammar_corr)
ggscatter(rh_controls_orig, x = "MTG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "Grammar")

## RH orig (MTG_ATL & Repetition)
rh_controls_orig_MTG_ATL_repetition_corr <- cor.test(rh_controls_orig$MTG_ATL, rh_controls_orig$Repetition_100,
                                                     method = "pearson")
print(rh_controls_orig_MTG_ATL_repetition_corr)
ggscatter(rh_controls_orig, x = "MTG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "Repetition")

## LH nordic (MTG_ATL & BNT)
lh_controls_nordic_MTG_ATL_BNT_corr <- cor.test(lh_controls_nordic$MTG_ATL, lh_controls_nordic$BNT_100,
                                                method = "pearson")
print(lh_controls_nordic_MTG_ATL_BNT_corr)
ggscatter(lh_controls_nordic, x = "MTG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "BNT")

## LH nordic (MTG_ATL & PPVT)
lh_controls_nordic_MTG_ATL_PPVT_corr <- cor.test(lh_controls_nordic$MTG_ATL, lh_controls_nordic$PPVT_100,
                                                 method = "pearson")
print(lh_controls_nordic_MTG_ATL_PPVT_corr)
ggscatter(lh_controls_nordic, x = "MTG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "PPVT")

## LH nordic (MTG_ATL & Grammar)
lh_controls_nordic_MTG_ATL_grammar_corr <- cor.test(lh_controls_nordic$MTG_ATL, lh_controls_nordic$Grammar_Total_100,
                                                    method = "pearson")
print(lh_controls_nordic_MTG_ATL_grammar_corr)
ggscatter(lh_controls_nordic, x = "MTG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "Grammar")

## LH nordic (MTG_ATL & Repetition)
lh_controls_nordic_MTG_ATL_repetition_corr <- cor.test(lh_controls_nordic$MTG_ATL, lh_controls_nordic$Repetition_100,
                                                       method = "pearson")
print(lh_controls_nordic_MTG_ATL_repetition_corr)
ggscatter(lh_controls_nordic, x = "MTG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "Repetition")

## RH nordic (MTG_ATL & BNT)
rh_controls_nordic_MTG_ATL_BNT_corr <- cor.test(rh_controls_nordic$MTG_ATL, rh_controls_nordic$BNT_100,
                                                method = "pearson")
print(rh_controls_nordic_MTG_ATL_BNT_corr)
ggscatter(rh_controls_nordic, x = "MTG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH nordic (MTG_ATLL & PPVT)
rh_controls_nordic_MTG_ATL_PPVT_corr <- cor.test(rh_controls_nordic$MTG_ATL, rh_controls_nordic$PPVT_100,
                                                 method = "pearson")
print(rh_controls_nordic_MTG_ATL_PPVT_corr)
ggscatter(rh_controls_nordic, x = "MTG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "PPVT")

## RH nordic (MTG_ATL & Grammar)
rh_controls_nordic_MTG_ATL_grammar_corr <- cor.test(rh_controls_nordic$MTG_ATL, rh_controls_nordic$Grammar_Total_100,
                                                    method = "pearson")
print(rh_controls_nordic_MTG_ATL_grammar_corr)
ggscatter(rh_controls_nordic, x = "MTG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "Grammar")

## RH nordic (MTG_ATL & Repetition)
rh_controls_nordic_MTG_ATL_repetition_corr <- cor.test(rh_controls_nordic$MTG_ATL, rh_controls_nordic$Repetition_100,
                                                       method = "pearson")
print(rh_controls_nordic_MTG_ATL_repetition_corr)
ggscatter(rh_controls_nordic, x = "MTG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "Repetition",
          title = "Correlation of Control Right Hemisphere MTG-ATL Connectivity & Grammar for NORDIC-processed Data") +
  theme_classic()

# Correlation analysis for PPA (IFG-MTG) -----------------------------------------------
# Run correlations for IFG_MTG connectivity
## LH orig (IFG_MTG & BNT)
ppa_orig_left_IFG_MTG_BNT_corr <- cor.test(ppa_orig_left$IFG_MTG, ppa_orig_left$BNT_100,
                                              method = "pearson")
print(ppa_orig_left_IFG_MTG_BNT_corr)
ggscatter(ppa_orig, x = "IFG_MTG", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          cor.coef.size = 6,
          cex.lab=5, cex.axis=5, cex.main=5,
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "BNT",
          title = "Correlation of PPA Left Hemisphere IFG-MTG Connectivity & BNT for Original Data") +
  theme_classic()

## LH nordic (IFG_MTG & BNT)
ppa_nordic_left_IFG_MTG_BNT_corr <- cor.test(ppa_nordic_left$IFG_MTG, ppa_nordic_left$BNT_100,
                                           method = "pearson")
print(ppa_nordic_left_IFG_MTG_BNT_corr)
ggscatter(ppa_nordic_left, x = "IFG_MTG", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",cor.coef.size = 6,
          cex.lab=5, cex.axis=5, cex.main=5,
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "BNT",
          title = "Correlation of PPA Left Hemisphere IFG-MTG Connectivity & BNT for NORDIC-processed Data") +
  theme_classic()

## LH orig (IFG_MTG & PPVT)
ppa_orig_left_IFG_MTG_PPVT_corr <- cor.test(ppa_orig_left$IFG_MTG, ppa_orig_left$PPVT_100,
                                               method = "pearson")
print(ppa_orig_left_IFG_MTG_PPVT_corr)
ggscatter(ppa_orig_left, x = "IFG_MTG", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "PPVT")

## LH nordic (IFG_MTG & PPVT)
ppa_nordic_left_IFG_MTG_PPVT_corr <- cor.test(ppa_nordic_left$IFG_MTG, ppa_nordic_left$PPVT_100,
                                            method = "pearson")
print(ppa_nordic_left_IFG_MTG_PPVT_corr)
ggscatter(ppa_nordic_left, x = "IFG_MTG", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "PPVT")

## LH orig (IFG_MTG & Grammar)
ppa_orig_left_IFG_MTG_grammar_corr <- cor.test(ppa_orig_left$IFG_MTG, ppa_orig_left$Grammar_Total_100,
                                                  method = "pearson")
print(ppa_orig_left_IFG_MTG_grammar_corr)
ggscatter(ppa_orig_left, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "Grammar")

## LH nordic (IFG_MTG & Grammar)
ppa_nordic_left_IFG_MTG_grammar_corr <- cor.test(ppa_nordic_left$IFG_MTG, ppa_nordic_left$Grammar_Total_100,
                                               method = "pearson")
print(ppa_nordic_left_IFG_MTG_grammar_corr)
ggscatter(ppa_nordic_left, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "Grammar")

## LH orig (IFG_MTG & Repetition)
ppa_orig_left_IFG_MTG_repetition_corr <- cor.test(ppa_orig_left$IFG_MTG, ppa_orig_left$Repetition_100,
                                                     method = "pearson")
print(ppa_orig_left_IFG_MTG_repetition_corr)
ggscatter(ppa_orig_left, x = "IFG_MTG", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "Repetition")

## LH nordic (IFG_MTG & Repetition)
ppa_nordic_left_IFG_MTG_repetition_corr <- cor.test(ppa_nordic_left$IFG_MTG, ppa_nordic_left$Repetition_100,
                                                  method = "pearson")
print(ppa_nordic_left_IFG_MTG_repetition_corr)
ggscatter(ppa_nordic_left, x = "IFG_MTG", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "Repetition")

## RH orig (IFG_MTG & BNT)
ppa_orig_right_IFG_MTG_BNT_corr <- cor.test(ppa_orig_right$IFG_MTG,ppa_orig_right$BNT_100,
                                              method = "pearson")
print(ppa_orig_right_IFG_MTG_BNT_corr)
ggscatter(ppa_orig_right, x = "IFG_MTG", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "BNT")

## RH nordic (IFG_MTG & BNT)
ppa_nordic_right_IFG_MTG_BNT_corr <- cor.test(ppa_nordic_right$IFG_MTG,ppa_nordic_right$BNT_100,
                                            method = "pearson")
print(ppa_nordic_right_IFG_MTG_BNT_corr)
ggscatter(ppa_nordic_right, x = "IFG_MTG", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "BNT")

## RH orig (IFG_ATL & BNT)
ppa_orig_right_IFG_ATL_BNT_corr <- cor.test(ppa_orig_right$IFG_ATL, ppa_orig_right$BNT_100,
                                         method = "pearson")
print(ppa_orig_right_IFG_ATL_BNT_corr)
ggscatter(ppa_orig_right, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH nordic (IFG_ATL & BNT)
ppa_nordic_right_IFG_ATL_BNT_corr <- cor.test(ppa_nordic_right$IFG_ATL, ppa_nordic_right$BNT_100,
                                            method = "pearson")
print(ppa_nordic_right_IFG_ATL_BNT_corr)
ggscatter(ppa_nordic_right, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH orig (IFG_MTG & PPVT)
ppa_orig_right_IFG_MTG_PPVT_corr <- cor.test(ppa_orig_right$IFG_MTG, ppa_orig_right$PPVT_100,
                                               method = "pearson")
print(ppa_orig_right_IFG_MTG_PPVT_corr)
ggscatter(ppa_orig_right, x = "IFG_MTG", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "PPVT")

## RH nordic (IFG_MTG & PPVT)
ppa_nordic_right_IFG_MTG_PPVT_corr <- cor.test(ppa_nordic_right$IFG_MTG, ppa_nordic_right$PPVT_100,
                                             method = "pearson")
print(ppa_nordic_right_IFG_MTG_PPVT_corr)
ggscatter(ppa_nordic_right, x = "IFG_MTG", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "PPVT")

## RH orig (IFG_MTG & Grammar)
ppa_orig_right_IFG_MTG_grammar_corr <- cor.test(ppa_orig_right$IFG_MTG, ppa_orig_right$Grammar_Total_100,
                                                  method = "pearson")
print(ppa_orig_right_IFG_MTG_grammar_corr)
ggscatter(ppa_orig_right, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "Grammar")

## RH nordic (IFG_MTG & Grammar)
ppa_nordic_right_IFG_MTG_grammar_corr <- cor.test(ppa_nordic_right$IFG_MTG, ppa_nordic_right$Grammar_Total_100,
                                                method = "pearson")
print(ppa_nordic_right_IFG_MTG_grammar_corr)
ggscatter(ppa_nordic_right, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "Grammar")

## RH orig (IFG_MTG & Repetition)
ppa_orig_right_IFG_MTG_repetition_corr <- cor.test(ppa_orig_right$IFG_MTG, ppa_orig_right$Repetition_100,
                                                     method = "pearson")
print(ppa_orig_right_IFG_MTG_repetition_corr)
ggscatter(ppa_orig_right, x = "IFG_MTG", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "Repetition")

## RH orig (IFG_MTG & Repetition)
ppa_nordic_right_IFG_MTG_repetition_corr <- cor.test(ppa_nordic_right$IFG_MTG, ppa_nordic_right$Repetition_100,
                                                   method = "pearson")
print(ppa_nordic_right_IFG_MTG_repetition_corr)
ggscatter(ppa_nordic_right, x = "IFG_MTG", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "Repetition")

## LH nordic (IFG_MTG & PPVT)
lh_ppa_nordic_IFG_MTG_PPVT_corr <- cor.test(lh_ppa_nordic$IFG_MTG, lh_ppa_nordic$PPVT_100,
                                                 method = "pearson")
print(lh_ppa_nordic_IFG_MTG_PPVT_corr)
ggscatter(lh_ppa_nordic, x = "IFG_MTG", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "PPVT")

## LH nordic (IFG_MTG & Grammar)
lh_ppa_nordic_IFG_MTG_grammar_corr <- cor.test(lh_ppa_nordic$IFG_MTG, lh_ppa_nordic$Grammar_Total_100,
                                                    method = "pearson")
print(lh_ppa_nordic_IFG_MTG_grammar_corr)
ggscatter(lh_ppa_nordic, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "Grammar")

## LH nordic (IFG_MTG & Repetition)
ppa_nordic_left_IFG_MTG_repetition_corr <- cor.test(ppa_nordic_left$IFG_MTG, ppa_nordic_left$Repetition_100,
                                                       method = "pearson")
print(ppa_nordic_left_IFG_MTG_repetition_corr)
ggscatter(ppa_nordic_left, x = "IFG_MTG", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Left hemisphere)", ylab = "Repetition")

## RH nordic (IFG_MTG & BNT)
rh_ppa_nordic_IFG_MTG_BNT_corr <- cor.test(rh_ppa_nordic$IFG_MTG, rh_ppa_nordic$BNT_100,
                                                method = "pearson")
print(rh_ppa_nordic_IFG_MTG_BNT_corr)
ggscatter(rh_ppa_nordic, x = "IFG_MTG", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "BNT")

## RH nordic (IFG_MTG & PPVT)
rh_ppa_nordic_IFG_MTG_PPVT_corr <- cor.test(rh_ppa_nordic$IFG_MTG, rh_ppa_nordic$PPVT_100,
                                                 method = "pearson")
print(rh_ppa_nordic_IFG_MTG_PPVT_corr)
ggscatter(rh_ppa_nordic, x = "IFG_MTG", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "PPVT")

## RH nordic (IFG_MTG & Grammar)
rh_ppa_nordic_IFG_MTG_grammar_corr <- cor.test(rh_ppa_nordic$IFG_MTG, rh_ppa_nordic$Grammar_Total_100,
                                                    method = "pearson")
print(rh_ppa_nordic_IFG_MTG_grammar_corr)
ggscatter(rh_ppa_nordic, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "Grammar")

## RH nordic (IFG_MTG & Repetition)
rh_ppa_nordic_IFG_MTG_repetition_corr <- cor.test(rh_ppa_nordic$IFG_MTG, rh_ppa_nordic$Repetition_100,
                                                       method = "pearson")
print(rh_ppa_nordic_IFG_MTG_repetition_corr)
ggscatter(rh_ppa_nordic, x = "IFG_MTG", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "Repetition")

# Correlations for PPA (IFG-ATL) ------------------------------------------
# Run correlations for IFG_ATL connectivity
## LH orig (IFG_ATL & BNT)
ppa_orig_left_IFG_ATL_BNT_corr <- cor.test(ppa_orig_left$IFG_ATL, ppa_orig_left$BNT_100,
                                              method = "pearson")
print(ppa_orig_left_IFG_ATL_BNT_corr)
ggscatter(ppa_orig_left, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "BNT")

## LH nordic (IFG_ATL & BNT)
ppa_nordic_left_IFG_ATL_BNT_corr <- cor.test(ppa_nordic_left$IFG_ATL, ppa_nordic_left$BNT_100,
                                           method = "pearson")
print(ppa_nordic_left_IFG_ATL_BNT_corr)
ggscatter(ppa_nordic_left, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "BNT")

## LH orig (IFG_ATL & PPVT)
ppa_orig_left_IFG_ATL_PPVT_corr <- cor.test(ppa_orig_left$IFG_ATL, ppa_orig_left$PPVT_100,
                                               method = "pearson")
print(ppa_orig_left_IFG_ATL_PPVT_corr)
ggscatter(ppa_orig_left, x = "IFG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "PPVT")

## LH nordic (IFG_ATL & PPVT)
ppa_nordic_left_IFG_ATL_PPVT_corr <- cor.test(ppa_nordic_left$IFG_ATL, ppa_nordic_left$PPVT_100,
                                            method = "pearson")
print(ppa_nordic_left_IFG_ATL_PPVT_corr)
ggscatter(ppa_nordic_left, x = "IFG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "PPVT")

## LH orig (IFG_ATL & Grammar)
ppa_orig_left_IFG_ALT_grammar_corr <- cor.test(ppa_orig_left$IFG_ATL, ppa_orig_left$Grammar_Total_100,
                                                  method = "pearson")
print(ppa_orig_left_IFG_ALT_grammar_corr)
# Create a layout for the subplots
par(mfrow = c(1, 2))

# Plot correlation between ASHS and MOCATOTS
ggarrange(
ggscatter(ppa_orig_left, x = "IFG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-ATL Connectivity (Left hemisphere)", ylab = "Grammar",
          title = "Correlation between IFG-ATL Left Hemisphere & Grammar for Original Data",
          cor.coeff.args = list(label.y = 50),
          cor.coef.size = 6,
          cex.lab= 6, cex.axis= 6, cex.main= 6, cex.sub= 6) +
  theme_classic(),
ggscatter(ppa_nordic_left, x = "IFG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-ATL Connectivity (Left hemisphere)", ylab = "Grammar",
          title = "Correlation between IFG-ATL Left Hemisphere & Grammar for NORDIC-processed Data",
          cor.coeff.args = list(label.y = 70),
          cor.coef.size = 6,
          cex.lab= 6, cex.axis= 6, cex.main=6, cex.sub= 6) +
  theme_classic(),
ncol = 2)
## LH nordic (IFG_ATL & Grammar)
ppa_nordic_left_IFG_ALT_grammar_corr <- cor.test(ppa_nordic_left$IFG_ATL, ppa_nordic_left$Grammar_Total_100,
                                               method = "pearson")
print(ppa_nordic_left_IFG_ALT_grammar_corr)
ggscatter(ppa_nordic_left, x = "IFG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-ATL Connectivity (Left hemisphere)", ylab = "Grammar",
          title = "Correlation between IFG-ATL Left Hemisphere & Grammar for NORDIC-processed Data",
          cor.coeff.args = list(label.y = 70),
          cor.coef.size = 6,
          cex.lab= 6, cex.axis= 6, cex.main=6, cex.sub= 6) +
  theme_classic()

## LH orig (IFG_ATL & Repetition)
ppa_orig_left_IFG_ALT_repetition_corr <- cor.test(ppa_orig_left$IFG_ATL, ppa_orig_left$Repetition_100,
                                                     method = "pearson")
print(ppa_orig_left_IFG_ALT_repetition_corr)
ggscatter(ppa_orig_left, x = "IFG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "Repetition")

## LH nordic (IFG_ATL & Repetition)
ppa_nordic_left_IFG_ALT_repetition_corr <- cor.test(ppa_nordic_left$IFG_ATL, ppa_nordic_left$Repetition_100,
                                                    method = "pearson")
print(ppa_nordic_left_IFG_ALT_repetition_corr)
ggscatter(ppa_nordic_left, x = "IFG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "Repetition")

## RH orig (IFG_ATL & BNT)
ppa_orig_right_IFG_ATL_BNT_corr <- cor.test(ppa_orig_right$IFG_ATL, ppa_orig_right$BNT_100,
                                              method = "pearson")
print(ppa_orig_right_IFG_ATL_BNT_corr)
ggscatter(ppa_orig_right, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH orig (IFG_ATL & PPVT)
ppa_orig_right_IFG_ATL_PPVT_corr <- cor.test(ppa_orig_right$IFG_ATL, ppa_orig_right$PPVT_100,
                                               method = "pearson")
print(ppa_orig_right_IFG_ATL_PPVT_corr)
ggscatter(ppa_orig_right, x = "IFG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG-MTG Connectivity (Right hemisphere)", ylab = "PPVT")

## RH orig (IFG_ATL & Grammar)
ppa_orig_right_IFG_ATL_grammar_corr <- cor.test(ppa_orig_right$IFG_ATL, ppa_orig_right$Grammar_Total_100,
                                                  method = "pearson")
print(ppa_orig_right_IFG_ATL_grammar_corr)
ggscatter(ppa_orig_right, x = "IFG_MTG", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "Grammar")

## RH orig (IFG_ATL & Repetition)
ppa_orig_right_IFG_ATL_repetition_corr <- cor.test(ppa_orig_right$IFG_ATL, ppa_orig_right$Repetition_100,
                                                     method = "pearson")
print(ppa_orig_right_IFG_ATL_repetition_corr)
ggscatter(ppa_orig_right, x = "IFG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "Repetition")

## LH nordic (IFG_ATL & BNT)
ppa_nordic_left_IFG_ATL_BNT_corr <- cor.test(ppa_nordic_left$IFG_ATL, ppa_nordic_left$BNT_100,
                                                method = "pearson")
print(ppa_nordic_left_IFG_ATL_BNT_corr)
ggscatter(ppa_nordic_left, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "BNT")

## LH nordic (IFG_ATL & PPVT)
ppa_nordic_left_IFG_ATL_PPVT_corr <- cor.test(ppa_nordic_left$IFG_ATL, ppa_nordic_left$PPVT_100,
                                                 method = "pearson")
print(ppa_nordic_left_IFG_ATL_PPVT_corr)
ggscatter(ppa_nordic_left, x = "IFG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "PPVT")

## LH nordic (IFG_ATL & Grammar)
ppa_nordic_left_IFG_ATL_grammar_corr <- cor.test(ppa_nordic_left$IFG_ATL, ppa_nordic_left$Grammar_Total_100,
                                                    method = "pearson")
print(ppa_nordic_left_IFG_ATL_grammar_corr)
ggscatter(ppa_nordic_left, x = "IFG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "Grammar",
          title = "Correlation of PPA Left Hemisphere IFG-ATL Connectivity & Grammar for NORDIC-processed Data") +
  theme_classic()

## LH nordic (IFG_ATL & Repetition)
lh_ppa_nordic_IFG_ATL_repetition_corr <- cor.test(lh_ppa_nordic$IFG_ATL, lh_ppa_nordic$Repetition_100,
                                                       method = "pearson")
print(lh_ppa_nordic_IFG_ATL_repetition_corr)
ggscatter(lh_ppa_nordic, x = "IFG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Left hemisphere)", ylab = "Repetition")

## RH nordic (IFG_ATL & BNT)
ppa_nordic_right_IFG_ATL_BNT_corr <- cor.test(ppa_nordic_right$IFG_ATL, ppa_nordic_right$BNT_100,
                                                method = "pearson")
print(ppa_nordic_right_IFG_ATL_BNT_corr)
ggscatter(ppa_nordic_right, x = "IFG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH nordic (IFG_ATL & PPVT)
ppa_nordic_right_IFG_ATL_PPVT_corr <- cor.test(ppa_nordic_right$IFG_ATL, ppa_nordic_right$PPVT_100,
                                                 method = "pearson")
print(ppa_nordic_right_IFG_ATL_PPVT_corr)
ggscatter(ppa_nordic_right, x = "IFG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "PPVT")

## RH nordic (IFG_ATL & Grammar)
ppa_nordic_right_IFG_ATL_grammar_corr <- cor.test(ppa_nordic_right$IFG_ATL, ppa_nordic_right$Grammar_Total_100,
                                                    method = "pearson")
print(ppa_nordic_right_IFG_ATL_grammar_corr)
ggscatter(ppa_nordic_right, x = "IFG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "Grammar")

## RH nordic (IFG_ATL & Repetition)
ppa_nordic_right_IFG_ATL_repetition_corr <- cor.test(ppa_nordic_right$IFG_ATL, ppa_nordic_right$Repetition_100,
                                                       method = "pearson")
print(ppa_nordic_right_IFG_ATL_repetition_corr)
ggscatter(ppa_nordic_right, x = "IFG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IFG_ATL Connectivity (Right hemisphere)", ylab = "Repetition")

# Correlation analysis PPA (MTG-ATL) --------------------------------------
# Run correlations for MTG_ATL connectivity
## LH orig (MTG_ATL & BNT)
ppa_orig_left_MTG_ATL_BNT_corr <- cor.test(ppa_orig_left$MTG_ATL, ppa_orig_left$BNT_100,
                                              method = "pearson")
print(ppa_orig_left_MTG_ATL_BNT_corr)
ggscatter(ppa_orig_left, x = "MTG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "BNT",
          cor.coeff.args = list(label.y = 75),
          cor.coef.size = 6,
          cex.lab= 6, cex.axis= 6, cex.main=6, cex.sub= 6) +
  theme_classic()

## LH nordic (MTG_ATL & BNT)
ppa_nordic_left_MTG_ATL_BNT_corr <- cor.test(ppa_nordic_left$MTG_ATL, ppa_nordic_left$BNT_100,
                                           method = "pearson")
print(ppa_nordic_left_MTG_ATL_BNT_corr)
ggscatter(ppa_nordic_left, x = "MTG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "BNT",
          cor.coeff.args = list(label.y = 75),
          cor.coef.size = 6,
          cex.lab= 6, cex.axis= 6, cex.main=6, cex.sub= 6) +
  theme_classic()

## LH orig (MTG_ATL & PPVT)
lh_ppa_orig_MTG_ATL_PPVT_corr <- cor.test(lh_ppa_orig$MTG_ATL, lh_ppa_orig$PPVT_100,
                                               method = "pearson")
print(lh_ppa_orig_MTG_ATL_PPVT_corr)
ggscatter(lh_ppa_orig, x = "MTG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "PPVT")

## LH orig (MTG_ATL & Grammar)
lh_ppa_orig_MTG_ATL_grammar_corr <- cor.test(lh_ppa_orig$MTG_ATL, lh_ppa_orig$Grammar_Total_100,
                                                  method = "pearson")
print(lh_ppa_orig_MTG_ATL_grammar_corr)
ggscatter(lh_ppa_orig, x = "MTG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "Grammar")

## LH orig (MTG_ATL & Repetition)
lh_ppa_orig_MTG_ATL_repetition_corr <- cor.test(lh_ppa_orig$MTG_ATL, lh_ppa_orig$Repetition_100,
                                                     method = "pearson")
print(lh_ppa_orig_MTG_ATL_repetition_corr)
ggscatter(lh_ppa_orig, x = "MTG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "Repetition",
          title = "Correlation of PPA Left Hemisphere MTG-ATL Connectivity & Grammar for Original Data") +
  theme_classic()

## RH orig (MTG_ATL & BNT)
rh_ppa_orig_MTG_ATL_BNT_corr <- cor.test(rh_ppa_orig$MTG_ATL, rh_ppa_orig$BNT_100,
                                              method = "pearson")
print(rh_ppa_orig_MTG_ATL_BNT_corr)
ggscatter(rh_ppa_orig, x = "MTG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH orig (MTG_ATL & PPVT)
rh_ppa_orig_MTG_ATL_PPVT_corr <- cor.test(rh_ppa_orig$MTG_ATL, rh_ppa_orig$PPVT_100,
                                               method = "pearson")
print(rh_ppa_orig_MTG_ATL_PPVT_corr)
ggscatter(rh_ppa_orig, x = "MTG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "PPVT")

## RH orig (MTG_ATL & Grammar)
rh_ppa_orig_MTG_ATL_grammar_corr <- cor.test(rh_ppa_orig$MTG_ATL, rh_ppa_orig$Grammar_Total_100,
                                                  method = "pearson")
print(rh_ppa_orig_MTG_ATL_grammar_corr)
ggscatter(rh_ppa_orig, x = "MTG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "Grammar")

## RH orig (MTG_ATL & Repetition)
rh_ppa_orig_MTG_ATL_repetition_corr <- cor.test(rh_ppa_orig$MTG_ATL, rh_ppa_orig$Repetition_100,
                                                     method = "pearson")
print(rh_ppa_orig_MTG_ATL_repetition_corr)
ggscatter(rh_ppa_orig, x = "MTG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "Repetition",
          title = "Correlation of PPA Right Hemisphere MTG-ATL Connectivity & Repetition for Original Data") +
  theme_classic()

## LH nordic (MTG_ATL & BNT)
lh_ppa_nordic_MTG_ATL_BNT_corr <- cor.test(lh_ppa_nordic$MTG_ATL, lh_ppa_nordic$BNT_100,
                                                method = "pearson")
print(lh_ppa_nordic_MTG_ATL_BNT_corr)
ggscatter(lh_ppa_nordic, x = "MTG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "BNT",
          title = "Correlation of PPA Left Hemisphere MTG-ATL Connectivity & BNT for NORDIC-processed Data") +
  theme_classic()

## LH nordic (MTG_ATL & PPVT)
lh_ppa_nordic_MTG_ATL_PPVT_corr <- cor.test(lh_ppa_nordic$MTG_ATL, lh_ppa_nordic$PPVT_100,
                                                 method = "pearson")
print(lh_ppa_nordic_MTG_ATL_PPVT_corr)
ggscatter(lh_ppa_nordic, x = "MTG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "PPVT")

## LH nordic (MTG_ATL & Grammar)
lh_ppa_nordic_MTG_ATL_grammar_corr <- cor.test(lh_ppa_nordic$MTG_ATL, lh_ppa_nordic$Grammar_Total_100,
                                                    method = "pearson")
print(lh_ppa_nordic_MTG_ATL_grammar_corr)
ggscatter(lh_ppa_nordic, x = "MTG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "Grammar")

## LH nordic (MTG_ATL & Repetition)
lh_ppa_nordic_MTG_ATL_repetition_corr <- cor.test(lh_ppa_nordic$MTG_ATL, lh_ppa_nordic$Repetition_100,
                                                       method = "pearson")
print(lh_ppa_nordic_MTG_ATL_repetition_corr)
ggscatter(lh_ppa_nordic, x = "MTG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Left hemisphere)", ylab = "Repetition")

## RH nordic (MTG_ATL & BNT)
rh_controls_nordic_MTG_ATL_BNT_corr <- cor.test(rh_controls_nordic$MTG_ATL, rh_controls_nordic$BNT_100,
                                                method = "pearson")
print(rh_controls_nordic_MTG_ATL_BNT_corr)
ggscatter(rh_controls_nordic, x = "MTG_ATL", y = "BNT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "BNT")

## RH nordic (MTG_ATLL & PPVT)
rh_controls_nordic_MTG_ATL_PPVT_corr <- cor.test(rh_controls_nordic$MTG_ATL, rh_controls_nordic$PPVT_100,
                                                 method = "pearson")
print(rh_controls_nordic_MTG_ATL_PPVT_corr)
ggscatter(rh_controls_nordic, x = "MTG_ATL", y = "PPVT_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "PPVT")

## RH nordic (MTG_ATL & Grammar)
rh_controls_nordic_MTG_ATL_grammar_corr <- cor.test(rh_controls_nordic$MTG_ATL, rh_controls_nordic$Grammar_Total_100,
                                                    method = "pearson")
print(rh_controls_nordic_MTG_ATL_grammar_corr)
ggscatter(rh_controls_nordic, x = "MTG_ATL", y = "Grammar_Total_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "Grammar")

## RH nordic (MTG_ATL & Repetition)
rh_controls_nordic_MTG_ATL_repetition_corr <- cor.test(rh_controls_nordic$MTG_ATL, rh_controls_nordic$Repetition_100,
                                                       method = "pearson")
print(rh_controls_nordic_MTG_ATL_repetition_corr)
ggscatter(rh_controls_nordic, x = "MTG_ATL", y = "Repetition_100", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MTG_ATL Connectivity (Right hemisphere)", ylab = "Repetition",
          title = "Correlation of Control Right Hemisphere MTG-ATL Connectivity & Grammar for NORDIC-processed Data") +
  theme_classic()
