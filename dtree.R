library(readr)
library(mlr3verse)
library(ggplot2)
dat <- read_csv("Downloads/predictdeath1.csv")
dat<data.frame(dat)
dat<-na.omit(dat)
cont_covariates <- c("Age","High","Weight","BMI","CPBT","ACC","DHCAT","LNT","LPT","Reb","Plam","SymhosT","HossurgT","Sym.surgT","Root","LAD","LVEDD","LVESD","IVS","LVEF","ProxAo","Hb","WBC","Plt","N","cTnt","BNP","fibrinogen","D2","INR","Tbil","albumin","ALT","AST","urea","Cr","Na","K",data=dat)
cate_covariates <- c("Male","HBP","DM","CAD","CKD","COPD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","clopidogrel","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Shock","Tamponade","CPStatus","Emergency","ARR","Bentall","David","Wheat","FET","CABG","MVprocedures","TVprocedures","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","RNCD","LNCD","LRCD","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","RereCPB","AS","Mortality",data=dat)
#dat$MFS=factor(dat$MFS)
#dat$IscCerebral=factor(dat$IscCerebral)
#dat$IscSpinal=factor(dat$IscSpinal)
#dat$IscCoronary=factor(dat$IscCoronary)
#dat$IscMesenteric=factor(dat$IscMesenteric)
#dat$IscRenal=factor(dat$IscRenal)
#dat$IscLEM=factor(dat$IscLEM)
#dat$Hypotension=factor(dat$Hypotension)
#dat$Ventilation=factor(dat$Ventilation)
#dat$Tamponade=factor(dat$Tamponade)
#dat$CPStatus=factor(dat$CPStatus)
#dat$CABG=factor(dat$CABG)
#dat$FLRoot=factor(dat$FLRoot)
#dat$BACP=factor(dat$BACP)
#dat$Transfusion=factor(dat$Transfusion)
#dat$Shock=factor(dat$Shock)
#dat$RereCPB=factor(dat$RereCPB)
#dat$AS=factor(dat$AS)
task<-as_task_classif(dat, target="hosMortality")
#int_cols <- task$feature_types[type == "integer"]$id
#dat[, int_cols] <- lapply(dat[, int_cols], as.numeric)
#task$cbind(dat[, int_cols])
task
library(mlr3fselect)
#交叉验证的递归特征消除
optimizer = fs("rfecv",
               n_features =8,
               feature_number = 1)
learner = lrn("classif.rpart",
              predict_type = "prob")
instance = fsi(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 10),
  measures = msr("classif.acc"),
  terminator = trm("none"))
set.seed(123)
optimizer$optimize(instance)
library(ggplot2)
library(viridisLite)
library(mlr3misc)

data = as.data.table(instance$archive)[!is.na(iteration), ]
aggr = data[, list("y" = mean(unlist(.SD))), by = "batch_nr", .SDcols = "classif.acc"]
aggr[, batch_nr := 36 - batch_nr]

data[, n:= map_int(importance, length)]

ggplot(aggr, aes(x = batch_nr, y = y)) +
  geom_line(
    color = viridis(1, begin = 0.5),
    linewidth = 1) +
  geom_point(
    fill = viridis(1, begin = 0.5),
    shape = 21,
    size = 3,
    stroke = 0.5,
    alpha = 0.8) +
  geom_vline(
    xintercept = aggr[y == max(y)]$batch_nr,
    colour = viridis(1, begin = 0.33),
    linetype = 3
  ) +
  xlab("Number of Features") +
  ylab("Mean ACC") +
  scale_x_reverse() +
  theme_minimal()
as.data.table(instance$archive)[, list(features, classif.acc, iteration, importance)]
task$select(instance$result_feature_set)
task
final_score <- msrs(c("classif.auc",
                      "classif.ce",
                      "classif.acc",
                      "classif.precision",
                      "classif.recall",
                      "classif.sensitivity", 
                      "classif.specificity"))
learner$train(task)
print(learner$model)
dtree<-learner$model
pred1 <- learner$predict(task )#测试集
print(pred1)
conf <- pred1$confusion
print(conf)
autoplot(pred1, type = "roc")
pred1$score(msr("classif.acc")) #准确度
pred1$score(msr("classif.auc"))#auc曲面下面积
pred1$score(final_score)
library(kernelshap)
dat$hosMortality=factor(dat$hosMortality)
s <- kernelshap(dtree,X=dat[,-c(36)],bg_X=dat)#成功
s
library(shapviz)
# 构建可视化对象
shp <- shapviz(s) 
# 变量重要性
sv_importance(shp,kind = "bee") 
sv_importance(shp, kind = "beeswarm")
sv_importance(shp,fill = "#0085FF")
#CPBT+Plt+IscMesenteric+ACC+MFS+cTnt+AS+IscLEM
5#D2+CPBT+IsMesenteric+Plt+INR+CTnt+ACC+Plam
8#D2+CPBT+IsMesenteric+Plt+INR+CTnt+MFS+Hypotension