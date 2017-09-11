#********************************* 
#*    IFCS Data Challenge 
#*   Team: Fengmei Liu, Sucharu Gupta
#*   Date: 05082017
#*********************************

# libraries needed, pls install them first if you don't have them
#install.packages("missForest", dependencies=TRUE)
#install.packages("mice", dependencies=TRUE)
library(gdata)
library(mice) # for data imputation
library(randomForest) # for data imputation
library(VIM) # for data imputation
library(data.table) # for data processing
library(dplyr) # for data processing
library(missForest) # for imputation
library(Rtsne) #for visualization 
library(rgl) # for 3d visualization
library(clustMD) # for model based clustering
library(irlba) # for svd visualization
library(Matrix)
library(fpc) # for cluster.stats
library(GGally) # for one of ggplot
library(clustrd) # for MCA
library(cluster) # for daisy-gower distance
library(clustMixType) 


## Read in data
setwd("~/SJSU_ClASSES/Math285_Clustering/project")
dataset <- xls2csv("data_challenge2.xlsx")
data <- read.csv(dataset, header=TRUE,strip.white=TRUE)
varsset <- xls2csv("variables_data_challenge2.xlsx")
vars <- read.csv(varsset, header= TRUE, strip.white=TRUE)
colnames(data) <- sapply(vars$Variable, as.character) # to make the variables names same

## -------------------------------------------------------------
##  explore variable-            
##---------------------------------------------------------------

unique(vars$Domain)  # 6 domains (blank values for top 10 variables)

# Domain
vars[11:122,] %>% group_by(Domain) %>% dplyr::summarize(a=n())

#1             Activity    23
#2   Contextual factors    16
#3                 Pain    14
#4        Participation     8
#5 Physical\nimpairment    24
#6        Psychological    27

# Type
vars[11:122,] %>% group_by(Type) %>% dplyr::summarize(a=n())
#1         Continuous     8
#2        Dichotomous    64
#3 Multistate nominal     9
#4            Ordinal    30
#5       Trichotomous     1

# Group
vars[11:122,] %>% group_by(Baseline.questionnaire..BQ..or.phy..examination..PE..or.outcome..OU.
) %>% dplyr::summarize(a=n())
#1                                                                   BQ    77
#2                                                                   PE    35

# check missing values
par(mar = c(3,3,1,1))
hist(as.numeric(levels(vars$Missing....[11:nrow(vars)]))[vars$Missing....[11:nrow(vars)]], breaks=40,  right=FALSE, xlim = c(0,40), ann=FALSE, axes=FALSE,col = "royalblue")
axis(1, font.axis=1 , cex.axis=0.75, family = "sans")
axis(2,  font.axis=1, cex.axis=0.75, family = "sans")
title(main="Missing Proportion Distribution",font.main = 1, cex.main= 1, family="mono") # create title
mtext("Missing Proportion", 1,line=2,  cex=1, family="sans") # create x label
mtext("Frequency", 2, line=2, cex=1, family="sans") # create y label

# >30: 2
# >20: 2
# >10: 11

## -------------------------------------------------------------
#  Variables Processing 
##---------------------------------------------------------------

######  Special missing values
# sum of missing and non-missing
which((vars$N + vars$Missing..N) != 928)
 # 67 68 69 70 71 72 73 74 75 86 87 88 89 98 need extra treatements

# 1) Create extra level for 67 68 69 70 71 72 73 74 75 
colnames(data)[67:75] # see if the variables are what we want
stumiss <- which(data[,14] %in% c(4,5,6,7))
for (i in (67:75)){
	data[stumiss,i][is.na(data[stumiss,i])] <- 0
}
#data[stumiss,67:75] # check

# 2) Create extra level for 86 87 88 89
colnames(data)[86:89] # see if the variables are what we want
dbpmiss <- which(data[,100] %in% c(1))
# make 9 non-BP to be 1
data[dbpmiss,86][is.na(data[dbpmiss,86])] <- 1 
for (i in (87:89)){
	data[dbpmiss,i][is.na(data[dbpmiss,i])] <- 0
}

#####  Seperate ordinal to categorical and continuous 
## group to categorical and continuous 
# variable #18 we treat it as categorial
catvars <- c(which(vars$Type %in% c("Dichotomous","Multistate nominal","Trichotomous")),18)
bivars <- which(vars$Type %in% c("Dichotomous"))
nomivars <- c(which(vars$Type %in% c("Multistate nominal","Trichotomous")),18)
ordcontivars <- which(vars$Type %in% c("Ordinal","Continuous"))[-8]
ordvars <- which(vars$Type %in% c("Ordinal"))[-3]
contivars <- which(vars$Type %in% c("Continuous"))
# make categorical to be the right type
data[,catvars] <- lapply(data[,catvars], factor)
data[,ordvars] <- lapply(data[,ordvars], ordered)

#### summary scores will not incldued:  85, 122
#### missing values over 20% not included: 91, 98
#### check if any level dominate one variable, using 85% cutoff line
#  exclude: 35  57  69 93  94  95 100 109 111 112 113 114 115
nonconti.ind <- which(vars[1:122,]$Type !="Continuous")
maxprop <- NULL
for (i in nonconti.ind){
	maxprop[i] <- (max(summary(as.factor(data[,i])))/982)
}
print(which(maxprop >= 0.85))
vars$Variable[which(maxprop >= 0.85)]
# Start70, Rm190 , heartdisease, asthma, psychdisease, Domin_bp, Mdtnonreduce, Herndiscr, Herndiscl, Affstrenght, Affsens, Affdtr
# 35  57  69 93  94  95 100 109 111 112 113 114 115

## Select the features based on above analysis --------------------------
delvars <- c(1:10, 85, 122,91, 98, 35, 57 , 69 ,93 , 94,  95 ,100 ,109 ,111 ,112 ,113 ,114, 115)
data.new <- data[,-delvars]
# dim(data.new) # 928  95

## -------------------------------------------------------------
# Impute using MICE random forest
##---------------------------------------------------------------
library(mice)
library(randomForest)
library(VIM)
# !!!!! below code will take few minutes
imp.data <- mice(data.new, m=1, maxit=5, meth=c("rf"), seed=500)
# summary(imp.data)
comp.data <- complete(imp.data,1)

## -------------------------------------------------------------
# Clustering - 1. Transform all to categorical - clusCA
##---------------------------------------------------------------
bivars2 <- which(colnames(comp.data) %in% vars$Variable[bivars])
nomivars2 <- which(colnames(comp.data) %in% vars$Variable[nomivars])
ordvars2 <- which(colnames(comp.data) %in% vars$Variable[ordvars])
contivars2 <- which(colnames(comp.data) %in% vars$Variable[contivars])

mca.data <- comp.data
## transform continuous variables to categorical based on quantiles.
# lapply(mca.data[,contivars2],summary)

# Age
mca.data$Age <- ordered(cut(mca.data$Age, c(17, 34, 43, 52,66),include.lowest= TRUE))
levels(mca.data$Age) <- c("17-34", "34-43","43-52","52-66")
# Bhoej0/Height
mca.data$Bhoej0 <- ordered(cut(mca.data$Bhoej0, c(153.0,170.0,176.0,182.0,201.0),include.lowest= TRUE))
levels(mca.data$Bhoej0) <- c("153.0-170.0","170.0-176.0","176.0-182.0","182.0-201.0")
# Vasl0/LBP intensity
mca.data$Vasl0 <- ordered(cut(mca.data$Vasl0, c(0,5,7,8,10),include.lowest= TRUE))
levels(mca.data$Vasl0) <- c("0-4", "5-6","7","8-10")
# Okon0/Able to decrease pain
mca.data$Okon0 <- ordered(cut(mca.data$Okon0, c(0,2,4,6,10),include.lowest= TRUE))
levels(mca.data$Okon0) <- c("0-1", "2-3","4-5","6-10")
# Obeh0/Treatment not essential
mca.data$Obeh0 <- ordered(cut(mca.data$Obeh0, c(0,1,3,6,10),include.lowest= TRUE))
levels(mca.data$Obeh0) <- c("0","1-2", "3-5","6-10")
# Htil0/Self-rated general health
mca.data$Htil0 <- ordered(cut(mca.data$Htil0, c(5, 57, 68, 80,100),include.lowest= TRUE))
levels(mca.data$Htil0) <- c("5-57", "57-68","68-80","80-100")
# bmi/BMI
mca.data$bmi <- ordered(cut(mca.data$bmi, c(18, 23, 26, 28.3,60),include.lowest= TRUE))
levels(mca.data$bmi) <- c("18-23", "23-26","26-28.3","28.3-60")


# Try clusCA on all data
# allmca.out <- tune_clusmca(mca.data,nclusrange=3:12, ndimrange= 2:9, method="clusCA", criterion= "asw")

allmca.out <- tune_clusmca(mca.data,nclusrange=3, ndimrange= 2, method="clusCA", criterion= "asw")
plot(allmca.out$clusmcaobj,cludesc=TRUE)

## -------------------------------------------------------------
# Clustering - 2. clusCA -Domain Clustering
##---------------------------------------------------------------

contextual_vars <- which(colnames(comp.data) %in% vars$Variable[vars$Domain == "Contextual factors"])
activity_vars <- which(colnames(comp.data) %in% vars$Variable[vars$Domain == "Activity"])
pain_vars <- which(colnames(comp.data) %in% vars$Variable[vars$Domain == "Pain"])
parti_vars <- which(colnames(comp.data) %in% vars$Variable[vars$Domain == "Participation"])
physicalimp_vars <- which(colnames(comp.data) %in% vars$Variable[vars$Domain == "Physical\nimpairment"])
psycho_vars <- which(colnames(comp.data) %in% vars$Variable[vars$Domain == "Psychological"])

###### STEP1:   each domain fit clusCA clustering ------------

### Contextrual Factor domain -----------
# factors: 1  2  3  4  5  7  8 27 77 78 79 95
# names: "Bsex0","Age", "Budd0","Barb0", "Bfor0","Bhoej0","Bryg0","Htil0", "nootherdisease", "musculoskeldisease","otherchronicdisease", "bmi" 
contextual.data <- mca.data[,contextual_vars]
#contex.out <- tune_clusmca(contextual.data,nclusrange=3:12, ndimrange= 2:9, method="clusCA", criterion= "asw")
contex.out <- tune_clusmca(contextual.data,nclusrange=4, ndimrange= 2, method="clusCA", criterion= "asw")
contex.out <- clusmca(contextual.data,nclus=4, ndim= 2, method = "clusCA", alphak = .5)
plot(contex.out$clusmcaobj,cludesc=TRUE, what=c(TRUE, TRUE))
# 4 clusters, 2 dims
#       2     3     4     5     6     7     8     9
#3  0.186                                          
#4  0.211 0.186                                    
#5   0.17 0.199 0.181                              
#6  0.138 0.158 0.197 0.178                        
#7  0.107 0.133 0.152 0.194 0.177                  
#8  0.039 0.075 0.147 0.156 0.188 0.089            
#9  0.039 0.066 0.092 0.139 0.087 0.115 0.124      
#10 0.036  0.05 0.077 0.074 0.105 0.109 0.069 0.092
#11 0.034 0.066 0.053 0.075 0.066 0.067 0.103 0.127
#12 0.022 0.049 0.061 0.055 0.088 0.111 0.094 0.113

# Important factors: "Bsex0","Age", "Budd0","Barb0","Bhoej0","nootherdisease", "musculoskeldisease","otherchronicdisease"
# 1  2  3  4 8 77 78 79 
#
# X1: + male, age34-43, tall, no chronic disease, full-time work, low education
#      - female, old, short, has musculoskel/other chronic diseas, retired, high education
#      
# X2: + female, young, tall, no other chronic disease, work, high education.
#     - male, old(52-66), average height, has musculoskel/other chronic disease,(low education)
#
# cluster1: male, age34-43, tall, no other chronic disease, full-time work, lower education
# cluster2: female, age 17-34, (women average height), no other chronic disease, part-time work/student, higher education
# cluster3: male, old(52-66), average height, has musculoskel/other chronic disease,(low education)
# cluster4: female, old(52-66), (average height),has musculoskel/other chronic disease, retired, high education


sel_contxt_vars <- c("Bsex0","Age", "Budd0","Barb0","Bhoej0","nootherdisease", "musculoskeldisease","otherchronicdisease")


### Activity domain -----------
# factors: 21 22 29 30 31 32 33 34 35 36 37 39 40 41 43 45 48 54 61 73 74
# names: "Start30"   "Start40"   "Rm20"      "Rm30"      "Rm40"      "Rm50"      "Rm60"      "Rm70"  "Rm80"      "Rm90"      "Rm100"     "Rm120"     "Rm130"     "Rm140"     "Rm160"     "Rm180" "Rm220"     "Fabq50"    "Fabq130"   "facetsit"  "facetwalk"
activity.data <- mca.data[,activity_vars]
# activity.out <- tune_clusmca(activity.data,nclusrange=3:12, ndimrange= 2:9, method="clusCA", criterion= "asw")
activity.out <- tune_clusmca(activity.data,nclusrange=3, ndimrange= 2, method="clusCA", criterion= "asw")
plot(activity.out$clusmcaobj,cludesc=TRUE)
activity.out$critgrid

# 3 clusters, 2 dims
#       2     3     4     5     6     7     8     9
#3  0.247                                          
#4  0.177 0.203                                    
#5  0.189 0.153 0.185                              
#6  0.123 0.169 0.169 0.095                        
#7  0.115 0.103 0.094 0.103 0.105                  
#8  0.113 0.103 0.124   0.1 0.085 0.087            
#9  0.102 0.101 0.106  0.05 0.086 0.075 0.071      
#10 0.091 0.099 0.068 0.069 0.037 0.038 0.052 0.049
#11 0.075 0.074 0.118 0.024 0.057 0.028  0.03 0.044
#12 0.062 0.063 0.068 0.025 0.024 0.034 0.013 0.037

# Important factors:  "Rm30",  "Rm70" ,"Rm80","Rm100" , "Rm130" ,"Rm140","Rm180", "Rm220", "Start30","Start40"

# X1: + home activity slowly, self dress slowly , 
#     - home activity normal, self dress normal
# X2: + walk short distance, stand for short time, self-dress normal
#     - walk normal, stand normal, self-dress slowly

# cluster1: walk short distances because of back pain, stand for short time, do less home activity
# cluster2: not walk short distances, stand long,  get dressed slowly, trouble put on socks
# cluster3: dress normal, normal home activity


sel_activity_vars <- c("Rm30",  "Rm70" ,"Rm80","Rm100" , "Rm130" ,"Rm140","Rm180", "Rm220", "Start30","Start40")

### Pain domain -----------

pain.data <- mca.data[,pain_vars]
# pain.out <- tune_clusmca(pain.data,nclusrange=3:12, ndimrange= 2:9, method="clusCA", criterion= "asw")
pain.out <- tune_clusmca(pain.data,nclusrange=3, ndimrange= 2, method="clusCA", criterion= "asw")
plot(pain.out$clusmcaobj,cludesc=TRUE)
# pain.out$critgrid

# 3 clusters, 2 dims
#       2     3     4     5     6     7     8     9
#3  0.247                                          
#4  0.177 0.203                                    
#5  0.189 0.153 0.185                              
#6  0.123 0.169 0.169 0.095                        
#7  0.115 0.103 0.094 0.103 0.105                  
#8  0.113 0.103 0.124   0.1 0.085 0.087            
#9  0.102 0.101 0.106  0.05 0.086 0.075 0.071      
#10 0.091 0.099 0.068 0.069 0.037 0.038 0.052 0.049
#11 0.075 0.074 0.118 0.024 0.057 0.028  0.03 0.044
#12 0.062 0.063 0.068 0.025 0.024 0.034 0.013 0.037

# Important factors:  "Dlva0","Tlda0", "Vasl0","Vasb0", "Start10", "Start90", "Pain_dis" 
# X1 + pain has spred dwon to legs, leg pain is intense
# - pain has not spred down to legs, no leg pain
# X2 + LBP pain intensity is high, this pain episode last short
# - LBP pain intensity is low, this pain episode last long

# cluster1: pain has not spread down, leg pain 0, back pain only, very extreme backpain in last 2wks, this pain episode last 2 wks, pain caused#
# by phisical activity and phisical activity make it worse
# cluster2: pain has spred down to legs, leg pain is serious, back pain and pain in 1 or 2 legs, last year has >30 days LBP, caused by physical # work, pain intensity is above medium, pain bothersome in last 2 weeks, back/leg pain almost all the time
# cluster3: pain not spread down, not caused by work, not very bothersome in the last 2 wks, pain intensity is light, not pain all the time


sel_pain_vars <- c("Dlva0","Tlda0", "Vasl0","Vasb0", "Start10", "Start90", "Pain_dis")

### Particification domain -----------

parti.data <- mca.data[,parti_vars]
# parti.out <- tune_clusmca(parti.data,nclusrange=3:8, ndimrange= 2:7, method="clusCA", criterion= "asw")
parti.out <- tune_clusmca(parti.data,nclusrange=4, ndimrange= 2, method="clusCA", criterion= "asw")
plot(parti.out$clusmcaobj,cludesc=TRUE)
# parti.out$critgrid

# 4 clusters, 2 dims
#      2     3     4     5     6     7
#3 0.261                              
#4 0.264 0.209                        
#5 0.164 0.215 0.167                  
#6 0.215 0.164 0.245 0.209            
#7 0.132 0.143 0.167 0.128 0.075      
#8 0.126 0.147 0.199 0.074 0.086 0.157

# Important factors:  "Bfbe0", "Fabq70","Fabq90","Fabq100"

# X1: + work not make pain worse, phisical work load sitting/walking
#     - work make pain worse, heavey phisical work
# X2: + work is heavey, more sick leave and stay home time. 
#     - unsure if work is heavy, less sick leave and stay home time.

# cluster1: work not make pain worse, work is not heavy, physical workload is sitting/walking
# cluster2: not sure if it's work make the pain worse(but prune to believe so), prune to heavy work load
# cluster3: work caused pain and make it worse, heavey work load, long sick leave last month, stay home most of the time
# cluster4: unsure if work is heavy(prune to believe so), but work make pain worse, heavy work load

sel_parti_vars <- c("Bfbe0", "Fabq70","Fabq90","Fabq100")

### Physical impairment domain -----------

physi.data <- mca.data[,physicalimp_vars]
# physi.out <- tune_clusmca(physi.data,nclusrange=3:12, ndimrange= 2:9, method="clusCA", criterion= "asw")
physi.out <- tune_clusmca(physi.data,nclusrange=3, ndimrange= 2, method="clusCA", criterion= "asw")
plot(physi.out$clusmcaobj,cludesc=TRUE)
# physi.out$critgrid
# 3 clusters, 2 dims
#        2     3     4     5     6     7     8     9
#3   0.219                                          
#4    0.13  0.16                                    
#5   0.122 0.146 0.147                              
#6    0.07 0.141 0.146  0.15                        
#7    0.07 0.107  0.11 0.149 0.145                  
#8   0.041 0.102 0.125 0.114 0.149 0.147            
#9   0.026 0.091 0.101 0.105  0.11  0.14 0.138      
#10  0.003 0.085 0.098 0.089 0.102 0.138 0.112 0.134
#11 -0.002 0.054 0.082 0.054 0.114 0.128 0.126 0.107
#12 -0.006 0.038  0.04 0.096 0.098 0.125 0.105 0.115

# Important factors:  "Mdtpartlyreduce", "Romrotl","siP4_comb","sicompres_comb","sigaens_comb", "Mdtdysfunc","Romext", "Romsidegll", "Romflex"

# X1:   +  no pain on AROM, 

#       - leg pain on  AROM test

# X2: + negative on SI-joint tests, no pain on palpation
#     - postive on SI-joint test, back pain on AROM test

# cluster1: negative on SI-joint tests, no pain on palpation, no pain on AROM, 
# cluster2: postive on SI-joint test, back pain on AROM
# clsuter3: leg pain on AROM test

sel_physi_vars <- c("Mdtpartlyreduce", "Romrotl","siP4_comb","sicompres_comb","sigaens_comb", "Mdtdysfunc","Romext", "Romsidegll", "Romflex")

### Psychological impairment domain -----------
# psycho_vars
# factors: 15 16 17 18 23 24 25 42 44 47 49 52 53 59 60 62 63 64 65 66 67 68 69 70 71
# names: "Okon0"   "Okom0"   "Oens0"   "Obeh0"   "Start50" "Start60" "Start80" "Rm150"   "Rm170"  "Rm210"   "Rm230"   "Fabq30"  "Fabq40"  "Fabq110" "Fabq120" "Fabq140" "Mdi1"    "mdi2"   "Mdi3"    "Mdi4"    "Mdi5"    "Mdi7"    "Mdi8"    "Mdi9"    "Mdi10"    
psycho.data <- mca.data[,psycho_vars]
# psycho.out <- tune_clusmca(psycho.data,nclusrange=3:12, ndimrange= 2:9, method="clusCA", criterion= "asw")
psycho.out <- tune_clusmca(psycho.data,nclusrange=3, ndimrange= 2, method="clusCA", criterion= "asw")
plot(psycho.out$clusmcaobj,cludesc=TRUE)
# psycho.out$critgrid

# 3 clusters, 2 dims
#        2      3      4      5     6     7     8     9
#3   0.154                                             
#4   0.076  0.142                                      
#5   0.061  0.066  0.056                               
#6   0.024  0.056   0.05  0.049                        
#7   0.015  0.058  0.033  0.046  0.04                  
#8  -0.009  0.026  0.009  0.031 0.025 0.031            
#9   -0.04  0.014   0.03  0.027  0.02 0.024 0.017      
#10 -0.017  0.007 -0.003  0.018 0.006 0.021 0.017  0.01
#11 -0.029 -0.006   0.01  0.021 0.002 0.037 0.018 0.006
#12 -0.045 -0.016      0 -0.004 0.012 -0.02 0.006 0.008

# Important factors:  "Fabq120","Fabq140", "Mdi1" ,"mdi2","Mdi3" ,"Mdi4","Mdi5", "Mdi8"

# X1  + good mode and feel energy, good conscience, good sleep
#     - bad mode and feel less energy, bad conscience, bad sleep
# X2  + not lose interest in daily activities, psychologically believe should do activity
#      - lose interest in daily activities, spycho believe should not do activities
# cluster1: feel good and energy most of the time, good conscience, good sleep, should do some activity
# cluster2: feel bad and less energy some of the time, some guilty, socaliy isolated
# cluster3: feel bad and less energy most of the time, feel restless, have trouble sleep

sel_psycho_vars <- c("Fabq120","Fabq140", "Mdi1" ,"mdi2","Mdi3" ,"Mdi4","Mdi5", "Mdi8")

## -------------------------------------------------------------
# Clustering - 3. RKM - ALL Clustering
##---------------------------------------------------------------

###### STEP2:   use the output each domain as input of new RKM clustering ----------------------------
## functions from R library clustrd   https://github.com/cran/clustrd/tree/master/R
# 1)  from https://github.com/cran/clustrd/tree/master/R
clu_means<-function(x, id, disp=TRUE, center=TRUE, scale=TRUE){

  

  clu = NULL

  funs = NULL

  

  x = data.frame(scale(as.matrix(x), center = center, scale = scale))

  

  p=ncol(x)

  gm=apply(x,2,mean)

  

  id=factor(id)

  csize=as.vector(table(id)/sum(table(id)))

  

  x$clu=id

  clum=(x %>% group_by(clu) %>% summarise_all(funs(mean)))

  

  am=rbind(clum[,-1],gm)

  bm=data.frame(t(am))

  names(bm)=c(paste("C",1:nrow(clum),sep=""),"all")

  bm$names=row.names(bm)

  

  par_bm=data.frame(t(bm[-ncol(bm)]))

  gnam=paste(names(bm)[-ncol(bm)]," (",round(csize*100,digits=1),"%",")",sep="")

  #  cnm=paste(cnames,": ",round(csize*100,2),"%",sep="")

  

  gnam[length(gnam)] = "all"

  par_bm$clusters=gnam

  par_bm$csize=c(csize,1/length(csize))

  

  gg_color_hue <- function(n) {

    hues = seq(15, 375, length = n + 1)

    hcl(h = hues, l = 65, c = 100)[1:n]

  }

  

  mypal=gg_color_hue(length(csize))

  mypal=c("black",mypal)

  

  # if (scale == T) {

  #    pco=ggparcoord(par_bm[1:(dim(par_bm)[1]-1),],columns=1:p,groupColumn=p+1,scale="globalminmax",mapping = ggplot2::aes(size = 5*csize)) 

  #  } else {

  pco=ggparcoord(par_bm,,columns=1:p,groupColumn=p+1,scale="globalminmax",mapping = ggplot2::aes(size = 3*csize))

  #  }

  pco=pco+scale_size_identity()

  pco=pco+scale_colour_manual(values=mypal)

  

  #  if (scale == T) {

  #    pco=pco+geom_vline(xintercept=1:p,alpha=.5) + xlab("variables") + ylab("z-score") 

  #  } else {

  pco=pco+geom_vline(xintercept=1:p,alpha=.1) + xlab("") + ylab("mean") + theme_classic()

  #  }

  

  return(pco)

  

}

# 2) from https://github.com/cran/clustrd/tree/master/R
outOfIndependence=function(data,Gvec,labs,nolabs=F,fixmarg=T,firstfew=0,minx=-2.5,maxx=2.5,segSize=4,textSize=6){

  #  require(ggplot2)

  #  require(dummies)

  value = NULL

  newplace = NULL

  lbls = NULL

  

  data=data.frame(data)

  data=dummy.data.frame(data, dummy.classes = "ALL")

  

  K=max(Gvec)

  C=matrix(0,nrow(data),max(Gvec))

  # print(dim(C))

  for(j in 1:max(Gvec)){

    C[which(Gvec==j),j]=1

  }

  

  P=t(data) %*% C

  n=nrow(data)

  

  #   B=t(data) %*% data

  #   

  P=P/sum(P)

  

  c=apply(P,2,sum)

  c=t(t(c))

  r=apply(P,1,sum)

  r=t(t(r))

  

  invsqDc=diag(as.vector(1/sqrt(c)))

  invsqDr=diag(as.vector(1/sqrt(r)))

  eP= r%*% t(c)

  devP=invsqDr %*% (P-eP) %*% invsqDc

  

  

  ###### HERE STARTS THE FOR LOOP

  dfP=list()

  sortOp=list()

  bp=list()

  

  colorPal=rainbow(K)

  

  for(jj in 1:K){

    #topfew=which(abs(devP[,jj]*sqrt(n))>1)

    #print(labs[topfew])

    dfP[[jj]]=data.frame(value=devP[,jj]*sqrt(n),place=1:nrow(devP),lbls=labs)

    sortOp[[jj]]=sort(abs(dfP[[jj]]$value),decreasing=T,index.return=T)

    #   sortOp2=sort(abs(dfP2$value),decreasing=T,index.return=T)

    #   sortOp3=sort(abs(dfP3$value),decreasing=T,index.return=T)  

    

    dfP[[jj]]=dfP[[jj]][sortOp[[jj]]$ix,]

    dfP[[jj]]$newplace=nrow(devP):1

    xran=c(min(dfP[[jj]]$value)-.5,max(dfP[[jj]]$value)+.5)

    if(firstfew>0){

      dfP[[jj]]=dfP[[jj]][1:firstfew,]#names(dfP[[jj]])

      dfP[[jj]]$newplace=firstfew:1

    }

    

    bbp=ggplot(data=dfP[[jj]], aes(x=value,y=newplace),labels=lbls)

    

    if(fixmarg==T){

      bbp=bbp+geom_segment(data=dfP[[jj]],aes(x=0,xend=value,y=newplace,yend=newplace),colour=colorPal[jj],size=segSize,alpha=.25)

      bbp=bbp+theme(legend.position="none")+xlab("")+ylab("")+xlim(c(minx,maxx))

      bbp=bbp+theme(axis.text.x  = element_text(size=textSize),axis.text.y  = element_text(size=textSize))

      if(firstfew==0){bbp=bbp+theme(axis.line=element_blank(),axis.ticks = element_blank())}

    }

    else{

      

      bbp=bbp+geom_segment(data=dfP[[jj]],aes(x=0,xend=value,y=newplace,yend=newplace),colour=colorPal[jj],size=segSize,alpha=.25)

      bbp=bbp+theme(legend.position="none")+xlab("")+ylab("")+xlim(xran)

      bbp=bbp+theme(axis.text.x  = element_text(size=textSize),axis.text.y  = element_text(size=textSize))

      if(firstfew==0){bbp=bbp+theme(axis.line=element_blank(),axis.ticks = element_blank())}

    }

    if(nolabs==F){

      bbp=bbp+geom_text(data=dfP[[jj]],aes(label=lbls),size=textSize)

    }

    bp[[jj]]=bbp

  }

  #   

  #  

  out=list()

  out$G=bp

  

  out

}
# 3) from  https://github.com/cran/clustrd/tree/master/R
clusval<-function(x,dst="full"){

  if(dst=="full"){

    if(class(x)=="cluspca"){

      data = scale(x$odata, center = x$center, scale = x$scale)

      oDist = daisy(data,metric="euclidean")

    }else{

      oDist=daisy(x$odata,metric="gower")

    }

  }else{

    oDist=daisy(x$obscoord,metric="euclidean")

  }

  

  clu_res=cluster.stats(d=oDist,x$cluID,wgap=F,sepindex=F,sepwithnoise=F)

  

  out=list()

  

  out$ch=clu_res$ch

  out$asw=clu_res$avg.silwidth

  #out$crit=x$criterion

  class(out) = "clusval"

  return(out)

}

####  overall MCA

domout.data <- cbind( contex.out$clusmcaobj$obscoord, activity.out$clusmcaobj$obscoord, pain.out$clusmcaobj$obscoord, parti.out$clusmcaobj$obscoord, physi.out$clusmcaobj$obscoord,psycho.out$clusmcaobj$obscoord)

# svd visualization
library(irlba)
domcomb.svd <- irlba(domout.data,3)
domcomb.svd.data <- domout.data %*% domcomb.svd$v

# r-tsne visualization
domcomb.rstne <- Rtsne(domout.data,3)

##   RKM  # use functions : clustval(), clu_means()
outpca2 =princomp(domout.data)
plot(outpca2)

# outRKM3 <- tune_cluspca(as.matrix(domout.data), nclusrange = 8:12, ndimrange = 5:10, criterion = "asw", dst = "full", alpha = NULL, method = "RKM", center = TRUE, scale = TRUE, rotation = "none", nstart = 10, smartStart = NULL, seed = 1234)

# iterate 2:20 clusters using RKM to decide number of clusters
RKMcrit <- c()
RKMasw <- c()
RKMch <- c()
for (i in (2:20)) {
	outRKM2 = cluspca(as.matrix(domout.data), i, 12, method = "RKM", rotation = "varimax",nstart=20)
	RKMcrit[i]<- outRKM2$criterion
	RKMasw[i] <- clusval(outRKM2)$asw
	RKMch[i] <- clusval(outRKM2)$ch
}

# make a plot of the 3 measures
dat <- cbind(RKMcrit/10000,RKMasw*3, RKMch/300)
matplot(dat, type = c("b"),pch=1:3,col = 1:3, cex=0.5, xlab = "Clusters", ylab= "Measures", ann=FALSE, axes=FALSE)
legend("topright", legend =c("Winthin Variance/10000","Silhouette*3","CH Index/300"), col=1:3, pch=1:3, cex = 0.5)
axis(1, font.axis=1 , at= 1:20, cex.axis=0.75, family = "sans", col ="grey")
axis(2,  font.axis=1, cex.axis=0.75, family = "sans", col="grey")
title(main="Choose Clusters with Different Measures",font.main = 1, cex.main= 1, family="sans") # create title
mtext("Clusters", 1,line=2,  cex=1, family="sans") # create x label
mtext("Measure Values", 2, line=2, cex=1, family="sans") # create y label

# We choose 8 clusters as the final number of clusters

outRKM812 = cluspca(as.matrix(domout.data), 8, 12, method = "RKM", rotation = "varimax", nstart=20)
library(GGally)
cdsc812 = clu_means(outRKM812$odata,outRKM812$cluID, center=outRKM812$center)
cdsc812

### meaning of each cluster
# see documents

# ids and labels
summary(as.factor(outRKM812$cluID))
#  1   2   3   4   5   6   7   8 
# 212 173 148 114 114  66  64  37 

## -------------------------------------------------------------
# Clustering - Visualization
##---------------------------------------------------------------
# using R-tsne for 2d embedding
domcomb.rstne <- Rtsne(domout.data,3)

plot(domcomb.rstne$Y[,2:3], col=outRKM812$cluID,pch=outRKM812$cluID,cex=0.5, xlab = "dim2", ylab="dim3" )
legend("topright", legend =1:8, col=1:8, pch=1:8, cex = 0.5)

# 3d visualization
plot3d(domcomb.rstne$Y, col=outRKM812$cluID,pch=outRKM812$cluID,cex=0.5)

## -------------------------------------------------------------
# Clustering - Evaluation
##---------------------------------------------------------------
#LBP intensity at 2weeks,3months,12months
#Verify the clustering results using first 10 columns of data

data.wlabel <- cbind(data[,2:10], outRKM812$cluID)
# Pain intensity changes
data.wlabel.va <- data.wlabel[,which(colnames(data.wlabel) %in% c("vasl2w","vasl3m", "vasl12m","outRKM812$cluID"))]
# md.pattern(data.wlabel)
data.wlabel.va.nona <- na.omit(data.wlabel.va)

vaslchange <- cbind (as.data.frame(comp.data %>% group_by(outRKM812$cluID)%>% dplyr::summarize(a=mean(Vasl0)))$a, as.data.frame(data.wlabel.va.nona %>% group_by(data.wlabel.va.nona[,4])%>% dplyr::summarize(a=mean(vasl2w)))$a, as.data.frame(data.wlabel.va.nona %>% group_by(data.wlabel.va.nona[,4])%>% dplyr::summarize(a=mean(vasl3m)))$a, as.data.frame(data.wlabel.va.nona %>% group_by(data.wlabel.va.nona[,4])%>% dplyr::summarize(a=mean(vasl12m)))$a)

matplot(t(vaslchange), type = c("b"),pch=1:8,col = 1:8, cex=0.5, ylab= "LBP Intensity", xlab ="Time Point", axes = FALSE)
axis(1, font.axis=1 , cex.axis=0.75, at = c(1,2,3,4), labels=c("original","2w","3m","12m"), family = "sans")
axis(2,  font.axis=1, cex.axis=0.75, family = "sans")
legend("topright", legend =1:8, col=1:8, pch=1:8, cex = 1)

# RM prop change
data.wlabel.rm <- data.wlabel[,which(colnames(data.wlabel) %in% c("rmprop2w","rmprop3m", "rmprop12m","outRKM812$cluID"))]
data.wlabel.rm.nona <- na.omit(data.wlabel.rm)

rmchange <- cbind (as.data.frame(data.wlabel.rm.nona %>% group_by(data.wlabel.rm.nona[,4])%>% dplyr::summarize(a=mean(rmprop2w)))$a, as.data.frame(data.wlabel.rm.nona %>% group_by(data.wlabel.rm.nona[,4])%>% dplyr::summarize(a=mean(rmprop3m)))$a, as.data.frame(data.wlabel.rm.nona %>% group_by(data.wlabel.rm.nona[,4])%>% dplyr::summarize(a=mean(rmprop12m)))$a)

matplot(t(rmchange), type = c("b"),pch=1:8,col = 1:8, cex=0.5,ylab= "RMProp", xlab ="Time Point", axes = FALSE)
axis(1, font.axis=1 , cex.axis=0.75, at = c(1,2,3), labels=c("2w","3m","12m"), family = "sans")
axis(2,  font.axis=1, cex.axis=0.75, family = "sans")
legend("topright", legend =1:8, col=1:8, pch=1:8, cex = 1)

# gen 
data.wlabel.gen <- data.wlabel[,which(colnames(data.wlabel) %in% c("gen2w","gen3m", "gen12m","outRKM812$cluID"))]
data.wlabel.gen.nona <- na.omit(data.wlabel.gen)

genchange <- cbind (as.data.frame(data.wlabel.gen.nona %>% group_by(data.wlabel.gen.nona[,4])%>% dplyr::summarize(a=mean(gen2w)))$a, as.data.frame(data.wlabel.gen.nona %>% group_by(data.wlabel.gen.nona[,4])%>% dplyr::summarize(a=mean(gen3m)))$a, as.data.frame(data.wlabel.gen.nona %>% group_by(data.wlabel.gen.nona[,4])%>% dplyr::summarize(a=mean(gen12m)))$a)

matplot(t(genchange), type = c("b"),pch=1:8,col = 1:8, cex=0.5, axes=FALSE)
axis(1, font.axis=1 , cex.axis=0.75, at = c(1,2,3), labels=c("2w","3m","12m"), family = "sans")
axis(2,  font.axis=1, cex.axis=0.75, family = "sans")
legend("topright", legend =1:8, col=1:8, pch=1:8, cex = 0.8)


