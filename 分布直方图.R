library(readr)
library(ggplot2)
library(titanic)
library(dplyr)
library(cowplot)
imp<- read.csv("~/Downloads/imputation.csv")
impall<- read.csv("~/Downloads/deathall.csv")
ggplot(imp, aes(CPBT)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'CPB time' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(CPBT)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'CPB time' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(ACC)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'ACC' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(ACC)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'ACC' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(LNT)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LNT' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(LNT)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LNT' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(LPT)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LBT' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(LPT)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LBT' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(Root)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Root' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(Root)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Root' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(LAD)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LAD' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(LAD)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LAD' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(LVEDD)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LVEDD' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(LVEDD)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LVEDD' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(LVESD)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LVESD' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(LVESD)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LVESD' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(IVS)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'IVS' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(IVS)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'IVS' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(LVEF)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LVEF' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(LVEF)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'LVEF' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(ProxAo)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'ProxAo' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(ProxAo)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'ProxAo' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(Hb)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Hb' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(Hb)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Hb' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(WBC)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'WBC' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(WBC)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'WBC' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(Plt)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Plt' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(Plt)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Plt' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(N)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'N' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(N)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'N' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(cTnt)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,1)+
  ggtitle("Variable 'cTnT' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(cTnt)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,1)+
  ggtitle("Variable 'cTnT' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(BNP)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'BNP' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(BNP)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'BNP' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(fibrinogen)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Fibrinogen' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(fibrinogen)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Fibrinogen' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(D2)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'D2' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(D2)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'D2' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(INR)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'INR' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(INR)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'INR' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(Tbil)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Tbil' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(Tbil)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Tbil' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(albumin)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Albumin' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(albumin)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Albumin' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(ALT)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,300)+
  ggtitle("Variable 'ALT' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(ALT)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,300)+
  ggtitle("Variable 'ALT' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(AST)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,300)+
  ggtitle("Variable 'AST' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(AST)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,300)+
  ggtitle("Variable 'AST' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(urea)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,30)+
  ggtitle("Variable 'Urea' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(urea)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,30)+
  ggtitle("Variable 'Urea' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(Cr)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,500)+
  ggtitle("Variable 'Cr' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(Cr)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(0,500)+
  ggtitle("Variable 'Cr' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(Na)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Na' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(Na)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  ggtitle("Variable 'Na' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(imp, aes(K)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(2.5,6.5)+
  ggtitle("Variable 'K' original distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(impall, aes(K)) +
  geom_histogram(color = "#000000", fill = "#2E9FDF") +
  xlim(2.5,6.5)+
  ggtitle("Variable 'K' imputed distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
