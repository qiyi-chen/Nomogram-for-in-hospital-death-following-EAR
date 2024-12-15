library(readr)
library(precrec)
library(rms)
library(rmda)
library(pROC)
library(caret)
library(gtsummary)
da <- read.csv("Downloads/predictdeath.csv")
da<data.frame(da)
da<-na.omit(da)
cont_covariates <- c("Age","High","Weight","BMI","CPBT","ACC","DHCAT","LNT","LPT","Reb","Plam","SymhosT","HossurgT","Sym.surgT","Root","LAD","LVEDD","LVESD","IVS","LVEF","ProxAo","Hb","WBC","Plt","N","cTnt","BNP","fibrinogen","D2","INR","Tbil","albumin","ALT","AST","urea","Cr","Na","K",data=da)
cate_covariates <- c("Male","HBP","DM","CAD","CKD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Shock","Tamponade","CPStatus","Emergency","ARR","Bentall","David","FET","CABG","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","RNCD","LNCD","LRCD","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","RereCPB","AS","90Mortality",data=da)
da$Male=factor(da$Male)
da$HBP=factor(da$HBP)
da$DM=factor(da$DM)
da$CAD=factor(da$CAD)
da$CKD=factor(da$CKD)
da$Smoking=factor(da$Smoking)
da$FamilyHistory=factor(da$FamilyHistory)
da$Heartsurgery=factor(da$Heartsurgery)
da$Stroke=factor(da$Stroke)
da$AFhistory=factor(da$AFhistory)
da$anticoagulation=factor(da$anticoagulation)
da$Warfarin=factor(da$Warfarin)
da$antiplatelet=factor(da$antiplatelet)
da$aspirin=factor(da$aspirin)
da$IMH=factor(da$IMH)
da$PAU=factor(da$PAU)
da$MFS=factor(da$MFS)
da$BAV=factor(da$BAV)
da$TEVARR=factor(da$TEVARR)
da$IscCerebral=factor(da$IscCerebral)
da$IscSpinal=factor(da$IscSpinal)
da$IscCoronary=factor(da$IscCoronary)
da$IscMesenteric=factor(da$IscMesenteric)
da$IscRenal=factor(da$IscRenal)
da$IscUEM=factor(da$IscUEM)
da$IscLEM=factor(da$IscLEM)
da$Hypotension=factor(da$Hypotension)
da$Ventilation=factor(da$Ventilation)
da$Tamponade=factor(da$Tamponade)
da$CPStatus=factor(da$CPStatus)
da$Emergency=factor(da$Emergency)
da$ARR=factor(da$ARR)
da$Bentall=factor(da$Bentall)
da$David=factor(da$David)
da$FET=factor(da$FET)
da$CABG=factor(da$CABG)
da$FLRoot=factor(da$FLRoot)
da$FLascending=factor(da$FLascending)
da$FLarch=factor(da$FLarch)
da$FLdescending=factor(da$FLdescending)
da$tearRoot=factor(da$tearRoot)
da$tearascending=factor(da$tearascending)
da$teararch=factor(da$teararch)
da$teardescending=factor(da$teardescending)
da$commissuredistachment=factor(da$commissuredistachment)
da$Sinusinvolve=factor(da$Sinusinvolve)
da$RCANeri=factor(da$RCANeri)
da$LCANeri=factor(da$LCANeri)
da$IA=factor(da$IA)
da$LCCA=factor(da$LCCA)
da$LSCA=factor(da$LSCA)
da$ReCPB=factor(da$ReCPB)
da$ReACC=factor(da$ReACC)
da$UACP=factor(da$UACP)
da$ReACC=factor(da$ReACC)
da$UACP=factor(da$UACP)
da$BACP=factor(da$BACP)
da$Transfusion=factor(da$Transfusion)
da$AI=factor(da$AI)
da$hydropericardium=factor(da$hydropericardium)
da$Shock=factor(da$Shock)
da$RereCPB=factor(da$RereCPB)
da$AS=factor(da$AS)
set.seed(109)
trainid<-createDataPartition(y=da$hosMortality,p=0.70,list=F)
traindata<-da[trainid, ]
#write.csv(traindata,file= "predictdeath1.csv",row.names = F) #导出traindata
testdata<-da[-trainid,]
#write.csv(testdata,file= "predictdeath2.csv",row.names = F)
#单、多因素L
library(scitb)
allVars<-c("Age","High","Weight","BMI","CPBT","ACC","DHCAT","LNT","LPT","Reb","Plam","SymhosT","HossurgT","Sym.surgT","Root","LAD","LVEDD","LVESD","IVS","LVEF","ProxAo","Hb","WBC","Plt","N","cTnt","BNP","fibrinogen","D2","INR","Tbil","albumin","ALT","AST","urea","Cr","Na","K","Male","HBP","DM","CAD","CKD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Tamponade","CPStatus","Emergency","ARR","Bentall","David","FET","CABG","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","Shock","RereCPB","AS")
fvars<-c("Male","HBP","DM","CAD","CKD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Tamponade","CPStatus","Emergency","ARR","Bentall","David","FET","CABG","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","Shock","RereCPB","AS")
strata<-c("hosMortality")
table1<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=da) #自动检验正态性
table1<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=da, 
               nonnormal=c("meal.cal"),atotest=F)  #指定非正态
table1
#write.csv(table1,file= "PpF1.csv",row.names = F) #导出table1
library(autoReg)
library(dplyr)
table2=gaze(hosMortality~.,data=da) %>% myft()
table2
table3=gaze(hosMortality~.,data=da,method=3) %>% myft()
table3
#traindata$hosMortality=factor(traindata$hosMortality)
#多个变量因子化,运行autoReg命令前，把分类变量因子化
fit1<-glm(hosMortality~MFS+IscCerebral+IscSpinal+IscCoronary+IscMesenteric+IscRenal+IscLEM+CPStatus+Hypotension+Ventilation+Tamponade+Shock+CABG+FLRoot+CPBT+ACC+BACP+Transfusion+Reb+Plam+IVS+WBC+Plt+N+cTnt+BNP+fibrinogen+D2+INR+albumin+AST+urea+Cr+RereCPB+AS,
          family=binomial(link = logit),
          data = traindata)
#install.packages("autoReg")
library(autoReg)
autoReg(fit1,uni=TRUE,threshold=0.05)
autoReg(fit1)
result1<-autoReg(fit1, uni=TRUE) %>% myft()
result1
fit<-glm(hosMortality~IscCerebral+IscSpinal+IscCoronary+IscMesenteric+IscRenal+IscLEM+CPStatus+Hypotension+Ventilation+Tamponade+Shock+CABG+FLRoot+CPBT+Transfusion+Reb+IVS+WBC+Plt+N+D2+INR+RereCPB,
         family=binomial(link = logit),
         data = traindata)
res<-tbl_regression(fit, exponentiate=T) #进行回归分析
res

fmlog<-as.formula(hosMortality~IscCerebral+IscMesenteric+CPStatus+CPBT+D2+INR+CABG)
model <- glm(fmlog,
             data = traindata,
             family = binomial)
traindata$predlog <- predict(model,type = "response")
testdata$predlog <- predict(newdata=testdata,model,type = "response")
#校准曲线
val.prob(traindata$predlog,traindata$hosMortality)
library(fbroc)#这个包在使用时需要把结果变量变为逻辑型：
trainoutcome <- ifelse(traindata$hosMortality == 0,FALSE,TRUE)
testoutcome<- ifelse(testdata$hosMortality == 0,FALSE,TRUE)
#然后1行代码即可实现
set.seed(123)
trainlog.boot <- boot.roc(pred = traindata$predlog,
                        true.class = trainoutcome,
                        n.boot = 1000)
testlog.boot <- boot.roc(pred = testdata$predlog,
                          true.class = testoutcome,
                          n.boot = 1000)
# 绘制bootstrap ROC曲线
perf(trainlog.boot,"auc",conf.level=0.95)

plot(trainlog.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
perf(testlog.boot,"auc",conf.level=0.95)
plot(testlog.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="firebrick",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)

###
fmdtree<-as.formula(hosMortality~D2+CPBT+IscMesenteric+Plt+INR+cTnt+MFS+Hypotension)
modeldtree <- glm(fmdtree,
             data = traindata,
             family = binomial)
traindata$preddtree <- predict(modeldtree,type = "response")
testdata$preddtree <- predict(newdata=testdata,modeldtree,type = "response")
#校准曲线
val.prob(traindata$preddtree,traindata$hosMortality)
library(fbroc)#这个包在使用时需要把结果变量变为逻辑型：
trainoutcome <- ifelse(traindata$hosMortality == 0,FALSE,TRUE)
testoutcome<- ifelse(testdata$hosMortality == 0,FALSE,TRUE)
#然后1行代码即可实现
set.seed(123)
traindtree.boot <- boot.roc(pred = traindata$preddtree,
                          true.class = trainoutcome,
                          n.boot = 1000)
testdtree.boot <- boot.roc(pred = testdata$preddtree,
                         true.class = testoutcome,
                         n.boot = 1000)
# 绘制bootstrap ROC曲线
perf(traindtree.boot,"auc",conf.level=0.95)

plot(traindtree.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
perf(testdtree.boot,"auc",conf.level=0.95)
plot(testdtree.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="firebrick",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
###
fmsvm<-as.formula(hosMortality~CPBT+CPStatus+Transfusion+Plt+IscMesenteric+D2+CABG+IscCerebral)
modelsvm <- glm(fmsvm,
                  data = traindata,
                  family = binomial)
traindata$predsvm <- predict(modelsvm,type = "response")
testdata$predsvm <- predict(newdata=testdata,modelsvm,type = "response")
#校准曲线
val.prob(traindata$predsvm,traindata$hosMortality)
library(fbroc)#这个包在使用时需要把结果变量变为逻辑型：
trainoutcome <- ifelse(traindata$hosMortality == 0,FALSE,TRUE)
testoutcome<- ifelse(testdata$hosMortality == 0,FALSE,TRUE)
#然后1行代码即可实现
set.seed(123)
trainsvm.boot <- boot.roc(pred = traindata$predsvm,
                          true.class = trainoutcome,
                          n.boot = 1000)
testsvm.boot <- boot.roc(pred = testdata$predsvm,
                         true.class = testoutcome,
                         n.boot = 1000)
# 绘制bootstrap ROC曲线
perf(trainsvm.boot,"auc",conf.level=0.95)

plot(trainsvm.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
perf(testsvm.boot,"auc",conf.level=0.95)
plot(testsvm.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="firebrick",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
###
fmxgb<-as.formula(hosMortality~D2+CPBT+IscMesenteric+CPStatus+Plt+ACC+AST+Cr)
modelxgb <- glm(fmxgb,
                data = traindata,
                family = binomial)
traindata$predxgb <- predict(modelxgb,type = "response")
testdata$predxgb <- predict(newdata=testdata,modelxgb,type = "response")
#校准曲线
val.prob(traindata$predxgb,traindata$hosMortality)
library(fbroc)#这个包在使用时需要把结果变量变为逻辑型：
trainoutcome <- ifelse(traindata$hosMortality == 0,FALSE,TRUE)
testoutcome<- ifelse(testdata$hosMortality == 0,FALSE,TRUE)
#然后1行代码即可实现
set.seed(123)
trainxgb.boot <- boot.roc(pred = traindata$predxgb,
                          true.class = trainoutcome,
                          n.boot = 1000)
testxgb.boot <- boot.roc(pred = testdata$predxgb,
                         true.class = testoutcome,
                         n.boot = 1000)
# 绘制bootstrap ROC曲线
perf(trainxgb.boot,"auc",conf.level=0.95)

plot(trainxgb.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
perf(testxgb.boot,"auc",conf.level=0.95)
plot(testxgb.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="firebrick",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
###
fmrf<-as.formula(hosMortality~D2+CPBT+IscMesenteric+Plt+INR+Tamponade+Reb+albumin)
modelrf <- glm(fmrf,
                data = traindata,
                family = binomial)
traindata$predrf <- predict(modelrf,type = "response")
testdata$predrf <- predict(newdata=testdata,modelrf,type = "response")
#校准曲线
val.prob(traindata$predrf,traindata$hosMortality)
library(fbroc)#这个包在使用时需要把结果变量变为逻辑型：
trainoutcome <- ifelse(traindata$hosMortality == 0,FALSE,TRUE)
testoutcome<- ifelse(testdata$hosMortality == 0,FALSE,TRUE)
#然后1行代码即可实现
set.seed(123)
trainrf.boot <- boot.roc(pred = traindata$predrf,
                          true.class = trainoutcome,
                          n.boot = 1000)
testrf.boot <- boot.roc(pred = testdata$predrf,
                         true.class = testoutcome,
                         n.boot = 1000)
# 绘制bootstrap ROC曲线
perf(trainrf.boot,"auc",conf.level=0.95)

plot(trainrf.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
perf(testrf.boot,"auc",conf.level=0.95)
plot(testrf.boot,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="firebrick",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
#DCA曲线
library(dcurves)
dca(hosMortality ~ predlog+predrf+preddtree+predxgb+predsvm,
    data = traindata,
    thresholds = seq(0, 1, by = 0.01)) %>%
  plot(smooth = TRUE)
####
library(rmda)
Fit.log<- decision_curve(hosMortality~IscCerebral+IscMesenteric+CPStatus+CPBT+D2+INR+CABG,
                         data = traindata,
                         family = binomial(link ='logit'),
                         thresholds= seq(0,1, by =0.01),
                         confidence.intervals = 0.95,
                         study.design ='case-control',
                         population.prevalence = 0.1) 
Fit.RF<- decision_curve(hosMortality~D2+CPBT+IscMesenteric+Plt+INR+Tamponade+Reb+albumin,
                         data = traindata,
                         family = binomial(link ='logit'),
                         thresholds= seq(0,1, by =0.01),
                         confidence.intervals = 0.95,
                         study.design ='case-control',
                         population.prevalence = 0.1) 
Fit.Dtree<- decision_curve(hosMortality~D2+CPBT+IscMesenteric+Plt+INR+cTnt+MFS+Hypotension,
                         data = traindata,
                         family = binomial(link ='logit'),
                         thresholds= seq(0,1, by =0.01),
                         confidence.intervals = 0.95,
                         study.design ='case-control',
                         population.prevalence = 0.1) 
Fit.XGBoost<- decision_curve(hosMortality~D2+CPBT+IscMesenteric+CPStatus+Plt+ACC+AST+Cr,
                         data = traindata,
                         family = binomial(link ='logit'),
                         thresholds= seq(0,1, by =0.01),
                         confidence.intervals = 0.95,
                         study.design ='case-control',
                         population.prevalence = 0.1) 
Fit.SVM<- decision_curve(hosMortality~CPBT+CPStatus+Transfusion+Plt+IscMesenteric+D2+CABG+IscCerebral,
                         data = traindata,
                         family = binomial(link ='logit'),
                         thresholds= seq(0,1, by =0.01),
                         confidence.intervals = 0.95,
                         study.design ='case-control',
                         population.prevalence = 0.1) 
plot_decision_curve(list(Fit.log,Fit.RF,Fit.Dtree,Fit.XGBoost,Fit.SVM),
                    curve.names=c('Fit.log Model','Fit.RF Model','Fit.Dtree Model','Fit.XGBoost Model','Fit.SVM Model'),
                    cost.benefit.axis =TRUE,col= c('red','blue','black','green','yellow'),
                    confidence.intervals=FALSE,
                    standardize = TRUE)
#安装基础包
install.packages("devtools")
devtools::install_github('yikeshu0611/ggDCA')
options(unzip ='internal')
#载入需要用到的R包
library(rms)
library(ggDCA)
library(survival)
AUDC(dca)
library(rms)
library(DynNom)
library(regplot)
regplot(modelsvm,
        plots = c("violin", "boxes"), ##连续性变量形状，可选"no plot" "density" "boxes" "ecdf" "bars" "boxplot" "violin" "bean" "spikes"；分类变量的形状，可选"no plot" "boxes" "bars" "spikes"
        observation = F,#da[1,], #用哪行观测，或者T F
        center = T, # 对齐变量
        subticks = T,
        droplines = T,#是否画竖线
        title = "nomogram",
        points = T, # 截距项显示为0-100
        odds = F, # 是否显示OR值
        showP = T, # 是否显示变量的显著性标记
        rank = "sd", # 根据sd给变量排序
        interval="confidence", # 展示可信区间
        clickable = F # 是否可以交互
)
#####计算总分####
da$predsvm <- predict(newdata=da,modelsvm,type = "response")
data2 <- data.frame(da,predsvm)
write.csv(data2,,file= "pdpoints1.csv",row.names = F)

library(nricens)
library(survival)
# 定义结局事件，0是存活，1是死亡
event = ifelse(traindata$hosMortality,0,1)

# 两个只由预测变量组成的矩阵
z.std = as.matrix(subset(traindata, select = c(D2,CPBT,IscMesenteric,Plt,INR,Tamponade,Reb,albumin)))#旧模型
z.new = as.matrix(subset(traindata, select = c(CPBT,IscMesenteric,CPStatus,Transfusion,D2,Plt,IscCerebral,CABG)))#新模型

# 建立2个模型
mstd = glm(hosMortality~D2+CPBT+IscMesenteric+Plt+INR+Tamponade+Reb+albumin, family = binomial(), data = traindata, x=TRUE)
mnew = glm(hosMortality~CPBT+IscMesenteric+CPStatus+Transfusion+D2+Plt+IscCerebral+CABG, family = binomial(), data = traindata, x=TRUE)

# 取出模型预测概率
# 取出模型预测概率
p.std = mstd$fitted.values
p.new = mnew$fitted.values
## 结果变量 + 两个模型得到的预测概率
nribin(event = traindata$hosMortality, p.std = p.std, p.new = p.new, 
       cut = c(0.20), 
       niter = 500, 
       updown = 'category')
#install.packages("XQuartz") #安装R包
library(PredictABEL)  

traindata$event <- traindata$hosMortality

reclassification(data = traindata,
                 cOutcome = 106, # 结果变量在哪一列
                 predrisk1 = p.std,
                 predrisk2 = p.new,
                 cutoff = c(0,0.20,1)
)
#网页计算器
library(DynNom)
library(magrittr)
DNbuilder(modelsvm,da)
