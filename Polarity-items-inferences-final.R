
###########################
# READ THE DATA ONLINE
###########################
library(lme4)
require(sciplot)
require(ggplot2)
require(plyr)
library(tidyr)
library(reshape2)
library(MuMIn)
library(stringr)
library(tidyverse)
library(brms) 
# option for Bayesian regression models:
# use all available cores for parallel computing
options(mc.cores = parallel::detectCores())
library(HDInterval)
library(faintr)
library(bayestestR)
options(scipen = 999)

theme_set(theme_bw())

Folder <- "https://github.com/milicaden/polarity-items-monotonicity-inferences/tree/master/Results%20files/"

listfiles = c(paste0(Folder, "Exp1Results1.csv"),paste0(Folder, "Exp1Results2.csv"),paste0(Folder, "Exp1Results3.csv"),
              paste0(Folder, "Exp2Results1.csv"),paste0(Folder, "Exp2Results2.csv"),paste0(Folder, "Exp2Results3.csv"),
              paste0(Folder, "Exp3Results1.csv"),paste0(Folder, "Exp3Results2.csv"),paste0(Folder, "Exp3Results3.csv"),
              paste0(Folder, "Exp4Results1.csv"),paste0(Folder, "Exp4Results2.csv"),paste0(Folder, "Exp4Results3.csv"), paste0(Folder, "Exp4Results4.csv")
              )

for (resultfile in listfiles) {
		DDt <- read.csv(resultfile) 
		DDt$EXPn <- str_extract(resultfile, "Exp[0-9]+")
		DDt$GROUP <- gsub(".*([0-9]).csv", "\\1", resultfile) 
		if (exists("DDA")) {DDA <- rbind(DDA, subset(DDt, select=names(DDA)))}
			else {DDA <- DDt}
 }

#Naming experiments 1-4
DDA$EXP <- factor(DDA$EXPn)
levels(DDA$EXP) <- c("NM","NMCons","DD", "DDSplit")


#Gather participants' data for each experiment (in particular info about number and gender)
participants <- dcast(unique(DDA[c("subject", "Qreponse_2_sex", "EXP")]), EXP~Qreponse_2_sex)

DD <- subset(DDA, Ireponse>-.5)

###############################################################
# SPLIT THE NAMES OF EACH ITEM INTO THE EXPERIMENTAL FACTORS
###############################################################

#Getting the info about the condition from Gname
tmp <- data.frame(do.call('rbind', strsplit(as.character(DD$Gname), "-", fixed=T)))
names(tmp) <- c("Monotonicity", "Quantifier", "PI", "Content", "Proto", "Increasing")
DD <- cbind(DD, tmp)
rm(tmp)

# SPLITING THE DATETIME INTO DATE AND TIME
tmp <- data.frame(do.call('rbind', strsplit(as.character(DD$date_time), "_", fixed=T)))
names(tmp) <- c("DATE", "TIME")
DD <- cbind(DD, tmp)
rm(tmp)

##################################################################
# Raw Ireponse between 0 and 1, we change Ireponse into a percentage
# "Complementary" response for DE environments
##################################################################

DD$Ireponse <- 100*DD$Ireponse
DD$Irep <- DD$Ireponse
DD$Irep[DD$Increasing=="D"] <- 100-DD$Ireponse[DD$Increasing=="D"]

#NB:Irep is the judgment of the upward entailing inference and 100-judgment of the downward entailing inference
#I.e. Irep a measure of how UE an environment is perceived.

###############################################################
# English native speakers
###############################################################
DD$Qreponse_25_NativeLang <- toupper(DD$Qreponse_25_NativeLang) 

table(DD$Qreponse_25_NativeLang)
ENGLISH <- c("ENGLISH", "AMERICAN_E", "USA", "ENGLISDH", "ENGLISHY")

#Sort out native English speakers from non-natives
nativesornot <- ddply(DD, c("subject", "EXP", "GROUP"), 
	function(df)c(nativespeaker=max(df$Qreponse_25_NativeLang %in% ENGLISH), participant=max(df$Qreponse_25_NativeLang !="")))

#Number of participants and native speakers per experiment
nativesperexp <- ddply(nativesornot, c("EXP"), 
	function(df)c(nativespeakers=sum(df$nativespeaker), participants=sum(df$participant)))

######################################################
# More participant information
######################################################

attach(DD)
infos.subject <- data.frame(
	SubID=tapply(as.character(subject), subject, max),
	Date=tapply(as.character(DATE), subject, max),
	Time=tapply(as.character(TIME), subject, max),
	IP=tapply(as.character(ip), subject, max),
	CODE=tapply(as.character(Qreponse_20_turk), subject, max),
	SEX=tapply(as.character(Qreponse_2_sex), subject, max),
	AGE=tapply(as.character(Qreponse_1_age), subject, max),
	LANG=tapply(as.character(Qreponse_25_NativeLang), subject, max),
	GROUP=tapply(as.character(GROUP), subject, max),
	EXP=tapply(as.character(EXP), subject, max),
	meanDE_D=round(tapply(Ireponse[Monotonicity=="DE" & Increasing=="D"], subject[Monotonicity=="DE" & Increasing=="D"], mean)),
	meanUE_D=round(tapply(Ireponse[Monotonicity=="UE" & Increasing=="D"], subject[Monotonicity=="UE" & Increasing=="D"], mean)),
	meanDE_U=round(tapply(Ireponse[Monotonicity=="DE" & Increasing=="U"], subject[Monotonicity=="DE" & Increasing=="U"], mean)),
	meanUE_U=round(tapply(Ireponse[Monotonicity=="UE" & Increasing=="U"], subject[Monotonicity=="UE" & Increasing=="U"], mean)),
	meanRT=round(tapply(Irt, subject, mean),1),
	medianRT=round(tapply(Irt, subject, median),1),
	variability=round(tapply(Ireponse, subject, sd))
	)
detach(DD)

##################################################################
# Excluding participants: response times and DE/UE responses
##################################################################
BADSUBJ <- subset(infos.subject, (meanDE_D <= meanUE_D)|(meanUE_U <= meanDE_U))$SubID

#Exclude non-native speakers and the subjects who don't distinguish enough DE from UE env.
DD$KEEPSUBJECTS <- DD$Qreponse_25_NativeLang %in% ENGLISH  & !(DD$subject %in% BADSUBJ)
DDD <- subset(DD, KEEPSUBJECTS)

#Check out again participants information (how many remains post exlusions, and their gender)
participantsfinal <- dcast(unique(DDD[c("subject", "Qreponse_2_sex", "EXP")]), EXP~Qreponse_2_sex)

#Maximum and minimum time acceptable to give a response
Irtmax <- 10
Irtmin <- 1.4

#Proportion of responses given below the maximum and above the minimum response time per experiment
belowmaxrt <- ddply(DDD, c("EXP"), 
                    function(df)c(belowmax=length(df[df$Irt<Irtmax,]$Irt)/length(df$Irt)))

aboveminrt <- ddply(DDD, c("EXP"), 
                    function(df)c(abovemin=length(df[df$Irt>Irtmin,]$Irt)/length(df$Irt)))

#Subsetting the df after response time cleaning
DDDD <- subset(DDD, Irt < Irtmax & Irt > Irtmin & Bname %in% c("target", " target", " target", "\ttarget"))


###############################################################
# Graph - plotting Urating and Drating on separate axes, quantifiers collapse
###############################################################
#Aggregate reponses per subject+EXP+Monotonicity+PI+Increasing
individualmean <- aggregate(Ireponse~ subject+EXP+Monotonicity+PI+Increasing, data=DDDD, mean, na.rm=T)

#Separate by Inference direction
individualdirectional <- spread(individualmean, key = Increasing, value = Ireponse)

#Aggregate reponses per EXP+Monotonicity+PI
directionalresponsemean <- ddply(individualdirectional, c("EXP", "Monotonicity", "PI"),
             function(df)c(DErating=mean(df$D, na.rm=T), DEse=se(df$D, na.rm=T), UErating=mean(df$U, na.rm=T), UEse=se(df$U, na.rm=T)))


#relabel some things to make the graph easier to read
directionalresponsemean$EXP <- factor(directionalresponsemean$EXP,
                    levels = c("DD", "DDSplit", "NM", "NMCons"),
                    labels = c("Experiment 3", "Experiment 4", "Experiment 1", "Experiment 2"))

directionalresponsemean$PI <- factor(directionalresponsemean$PI,
                   levels = c("N", "O", "P"),
                   labels = c("NPI", "No PI", "PPI"))

directionalresponsemean$Monotonicity <- factor(directionalresponsemean$Monotonicity,
                           levels = c("DD", "DE", "NM", "UE"),
                           labels = c("DN", "DE", "NM", "UE"))


#Graph for experiment 1 and 2
png("Experiments1and2.png", width = 160, height = 100, units='mm', res = 300)
p <- ggplot(subset(directionalresponsemean, EXP == "Experiment 1"|EXP == "Experiment 2"), aes(x=DErating, y=UErating)) +
  geom_point(aes(shape=Monotonicity, fill=PI), size=4, alpha=.9)+
  scale_shape_manual(values=c(21, 22, 23, 24))+
  #scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  scale_fill_manual(values=c(NPI='green',`No PI`='gray78', PPI='red'))+
  guides(fill=guide_legend(override.aes=list(colour=c(NPI='green',`No PI`='gray78', PPI='red'))))+ #changes colors in legend
  geom_errorbar(aes(ymin = UErating-UEse,ymax = UErating+UEse)) + 
  geom_errorbarh(aes(xmin = DErating-DEse,xmax = DErating+DEse))+
  #facet_grid(.~ EXP, scales="free")+
  facet_grid(.~ EXP)+
  xlab("DE-rating") +
  ylab("UE-rating") +
  xlim(0, 100)+
  ylim(0, 100)+
  coord_fixed(ratio = 1)
print(p)
dev.off()


#Graph for experiment 3 and 4
png("Experiments3and4.png", width = 160, height = 100, units='mm', res = 300)
p <- ggplot(subset(directionalresponsemean, EXP == "Experiment 3"|EXP == "Experiment 4"), aes(x=DErating, y=UErating)) +
  geom_point(aes(shape=Monotonicity, fill=PI), size=4, alpha=.9)+
  scale_shape_manual(values=c(24, 21, 22, 23))+
  #scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  scale_fill_manual(values=c(NPI='green',`No PI`='gray78', PPI='red'))+
  guides(fill=guide_legend(override.aes=list(colour=c(NPI='green',`No PI`='gray78', PPI='red'))))+
  geom_errorbar(aes(ymin = UErating-UEse,ymax = UErating+UEse)) + 
  geom_errorbarh(aes(xmin = DErating-DEse,xmax = DErating+DEse))+
  facet_grid(.~ EXP)+
  xlab("DE-rating") +
  ylab("UE-rating") +
  xlim(0, 100)+
  ylim(0, 100)+
  coord_fixed(ratio = 1)
print(p)
dev.off()

########################################
######General data overview stats#######
########################################

#Calculate the training items response means
training <- ddply(subset(DDD, Bname %in% c("training")), c("EXP", "Content"), 
                      function(df)c(trainingmean=mean(df$Irep)))

#Calculating average DE and UE ratings per experiment collapsing PI
DEUErating <- ddply(individualdirectional, c("EXP", "Monotonicity"),
              function(df)c(DErating=mean(df$D, na.rm=T), DEse=sd(df$D, na.rm=T), UErating=mean(df$U, na.rm=T), UEse=sd(df$U, na.rm=T)))

#Calculating average DE and UE ratings collapsing PI and EXP
DEUEcollapsed <- ddply(individualdirectional, c("Monotonicity"),
                    function(df)c(DErating=mean(df$D, na.rm=T), DEse=sd(df$D, na.rm=T), UErating=mean(df$U, na.rm=T), UEse=sd(df$U, na.rm=T)))

#Calculating average per PI per experiment per environment
individualirep <- aggregate(Irep~ subject+EXP+Monotonicity+PI, data=DDDD, mean, na.rm=T)
PIinNM <- ddply(individualirep, c("EXP", "Monotonicity", "PI"),
                    function(df)c(Irepmean=mean(df$Irep, na.rm=T), Irepse=sd(df$Irep, na.rm=T)))


###############################################################
# Analyses
###############################################################

expts = c("NM", "NMCons", "DD", "DDSplit")
envts = c("NM", "DD")

PIs = c("O", "N", "P")

AnDataSet <- subset(DDDD, EXP %in% expts & Monotonicity %in% envts & PI %in% PIs)
AnDataSet$subject <- as.factor(AnDataSet$subject)
AnDataSet$EXP <- as.factor(AnDataSet$EXP)

###########################################
#LMMs: Combined measure
###########################################
##NPI in NM env in Exp1
###########################################

a1df <- subset(AnDataSet, EXP == "NM" & Monotonicity == "NM" & PI %in% c("O", "N"))
a1df[] <- lapply(a1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(a1df$PI) <- contr.sum(2)
contrasts(a1df$Quantifier) <- contr.sum(2)
contrasts(a1df$Increasing) <- contr.sum(2)

#contrast coding: N:1, 0:-1
#Model with PI as predictor #earlier version iter = 20000
a1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ PI+ Increasing+ Quantifier| subject),
  iter = 8000, data = a1df, save_pars = save_pars(all = TRUE))
summary(a1)

# Probability that the presence of NPI lowers the judgments
post_samples_a1 = posterior_samples(a1)
mean(post_samples_a1$b_PI1 < 0)

# Summary of the prior
prior_summary(a1)

###########################################
##NPI in NM env in Exp2
###########################################
b1df <- subset(AnDataSet, EXP == "NMCons" & Monotonicity == "NM" & PI %in% c("O", "N"))
b1df[] <- lapply(b1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(b1df$PI) <- contr.sum(2)
contrasts(b1df$Quantifier) <- contr.sum(2)
contrasts(b1df$Increasing) <- contr.sum(2)

#contrast coding: N:1, 0:-1
#Model with PI as predictor
b1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ PI+ Increasing+ Quantifier| subject),
  data = b1df,
  iter = 12000, save_pars = save_pars(all = TRUE), control = list(max_treedepth = 15))
summary(b1)


# Probability that the presence of NPI lowers the judgments
post_samples_b1 = posterior_samples(b1)
mean(post_samples_b1$b_PI1 < 0)

# Summary of the prior
prior_summary(b1)

###########################################
##NPI in NM env in Exp3
###########################################
c1df <- subset(AnDataSet, EXP == "DD" & Monotonicity == "NM" & PI %in% c("O", "N"))
c1df[] <- lapply(c1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(c1df$PI) <- contr.sum(2)
contrasts(c1df$Quantifier) <- contr.sum(2)
contrasts(c1df$Increasing) <- contr.sum(2)

#contrast coding: N:1, 0:-1
#Model with PI as predictor
c1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ PI+ Increasing+ Quantifier| subject),
  data = c1df,
  iter = 8000, save_pars = save_pars(all = TRUE))
summary(c1)

# Probability that the presence of NPI lowers the judgments
post_samples_c1 = posterior_samples(c1)
mean(post_samples_c1$b_PI1 < 0)

# Summary of the prior
prior_summary(c1)

###########################################
##NPI in NM env in Exp4
###########################################
d1df <- subset(AnDataSet, EXP == "DDSplit" & Monotonicity == "NM" & PI %in% c("O", "N"))
d1df[] <- lapply(d1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(d1df$PI) <- contr.sum(2)
contrasts(d1df$Quantifier) <- contr.sum(2)
contrasts(d1df$Increasing) <- contr.sum(2)

#contrast coding: N:1, 0:-1
#Model with PI as predictor: in Exp 4, we cannot have by-subject PI slopes.
d1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = d1df,
  iter = 8000, save_pars = save_pars(all = TRUE))
summary(d1)

# Probability that the presence of NPI lowers the judgments
post_samples_d1 = posterior_samples(d1)
mean(post_samples_d1$b_PI1 < 0)

# Summary of the prior
prior_summary(d1)


###########################################
##NPI in DN env in Exp3
###########################################
e1df <- subset(AnDataSet, EXP == "DD" & Monotonicity == "DD" & PI %in% c("O", "N"))
e1df[] <- lapply(e1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(e1df$PI) <- contr.sum(2)
contrasts(e1df$Quantifier) <- contr.sum(2)
contrasts(e1df$Increasing) <- contr.sum(2)


#contrast coding: N:1, 0:-1
#Model with PI as predictor
e1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ PI+ Increasing+ Quantifier| subject),
  data = e1df,
  iter = 15000, save_pars = save_pars(all = TRUE))
summary(e1)

# Probability that the presence of NPI lowers the judgments
post_samples_e1 = posterior_samples(e1)
mean(post_samples_e1$b_PI1 < 0)

# Summary of the prior
prior_summary(e1)

###########################################
##NPI in DN env in Exp4
###########################################
f1df <- subset(AnDataSet, EXP == "DDSplit" & Monotonicity == "DD" & PI %in% c("O", "N"))
f1df[] <- lapply(f1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(f1df$PI) <- contr.sum(2)
contrasts(f1df$Quantifier) <- contr.sum(2)
contrasts(f1df$Increasing) <- contr.sum(2)
#contrast coding: N:1, 0:-1
#Model with PI as predictor
f1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = f1df,
  iter = 8000, save_pars = save_pars(all = TRUE))
summary(f1)


# Probability that the presence of NPI lowers the judgments
post_samples_f1 = posterior_samples(f1)
mean(post_samples_f1$b_PI1 < 0)

# Summary of the prior
prior_summary(f1)

###########################################
##PPI in NM env in Exp1
###########################################
g1df <- subset(AnDataSet, EXP == "NM" & Monotonicity == "NM" & PI %in% c("O", "P"))
g1df[] <- lapply(g1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
g1df$PI <- relevel(g1df$PI, "P")
contrasts(g1df$PI) <- contr.sum(2)
contrasts(g1df$Quantifier) <- contr.sum(2)
contrasts(g1df$Increasing) <- contr.sum(2)

#contrast coding: P:1, 0:-1
#Model with PI as predictor
g1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ PI+ Increasing+ Quantifier| subject),
  data = g1df,
  iter = 8000, save_pars = save_pars(all = TRUE))
summary(g1)

# Probability that the presence of PPI increases the judgments
post_samples_g1 = posterior_samples(g1)
mean(post_samples_g1$b_PI1 > 0)

# Summary of the prior
prior_summary(g1)


###########################################
##PPI in NM env in Exp2
###########################################

h1df <- subset(AnDataSet, EXP == "NMCons" & Monotonicity == "NM" & PI %in% c("O", "P"))
h1df[] <- lapply(h1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
h1df$PI <- relevel(h1df$PI, "P")
contrasts(h1df$PI) <- contr.sum(2)
contrasts(h1df$Quantifier) <- contr.sum(2)
contrasts(h1df$Increasing) <- contr.sum(2)

#contrast coding: P:1, 0:-1
#Model with PI as predictor
h1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ PI+ Increasing+ Quantifier| subject),
  data = h1df,
  iter = 20000, save_pars = save_pars(all = TRUE), control = list(max_treedepth = 15))
summary(h1)

# Probability that the presence of PPI increases the judgments
post_samples_h1 = posterior_samples(h1)
mean(post_samples_h1$b_PI1 > 0)

# Summary of the prior
prior_summary(h1)

###########################################
##PPI in NM env in Exp3
###########################################
i1df <- subset(AnDataSet, EXP == "DD" & Monotonicity == "NM" & PI %in% c("O", "P"))
i1df[] <- lapply(i1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
i1df$PI <- relevel(i1df$PI, "P")
contrasts(i1df$PI) <- contr.sum(2)
contrasts(i1df$Quantifier) <- contr.sum(2)
contrasts(i1df$Increasing) <- contr.sum(2)
#contrast coding: P:1, 0:-1
#Model with PI as predictor
i1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ PI+ Increasing+ Quantifier| subject),
  data = i1df,
  iter = 20000, save_pars = save_pars(all = TRUE), control = list(max_treedepth = 15))
summary(i1)


# Probability that the presence of PPI increases the judgments
post_samples_i1 = posterior_samples(i1)
mean(post_samples_i1$b_PI1 > 0)

# Summary of the prior
prior_summary(i1)

###########################################
##PPI in NM env in Exp4
###########################################
j1df <- subset(AnDataSet, EXP == "DDSplit" & Monotonicity == "NM" & PI %in% c("O", "P"))
j1df[] <- lapply(j1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
j1df$PI <- relevel(j1df$PI, "P")
contrasts(j1df$PI) <- contr.sum(2)
contrasts(j1df$Quantifier) <- contr.sum(2)
contrasts(j1df$Increasing) <- contr.sum(2)

#contrast coding: P:1, 0:-1
#Model with PI as predictor
j1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = j1df,
  iter = 8000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(j1)

# Probability that the presence of PPI increases the judgments
post_samples_j1 = posterior_samples(j1)
mean(post_samples_j1$b_PI1 > 0)

# Summary of the prior
prior_summary(j1)

###########################################
##PPI in DN env in Exp3
###########################################
k1df <- subset(AnDataSet, EXP == "DD" & Monotonicity == "DD" & PI %in% c("O", "P"))
k1df[] <- lapply(k1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
k1df$PI <- relevel(k1df$PI, "P")
contrasts(k1df$PI) <- contr.sum(2)
contrasts(k1df$Quantifier) <- contr.sum(2)
contrasts(k1df$Increasing) <- contr.sum(2)

#contrast coding: P:1, 0:-1
#Model with PI as predictor
k1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ PI+ Increasing+ Quantifier| subject),
  data = k1df,
  iter = 15000, save_pars = save_pars(all = TRUE))
summary(k1)


# Probability that the presence of PPI increases the judgments
post_samples_k1 = posterior_samples(k1)
mean(post_samples_k1$b_PI1 > 0)

# Summary of the prior
prior_summary(k1)

###########################################
##PPI in DN env in Exp4
###########################################
l1df <- subset(AnDataSet, EXP == "DDSplit" & Monotonicity == "DD" & PI %in% c("O", "P"))
l1df[] <- lapply(l1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
l1df$PI <- relevel(l1df$PI, "P")
contrasts(l1df$PI) <- contr.sum(2)
contrasts(l1df$Quantifier) <- contr.sum(2)
contrasts(l1df$Increasing) <- contr.sum(2)

#contrast coding: P:1, 0:-1
#Model with PI as predictor
l1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = l1df,
  iter = 8000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(l1)


# Probability that the presence of PPI increases the judgments
post_samples_l1 = posterior_samples(l1)
mean(post_samples_l1$b_PI1 > 0)

# Summary of the prior
prior_summary(l1)

#######################
##META-ANALYSIS
######################
###NPI in NM####
######################

maa1df <- subset(AnDataSet, Monotonicity == "NM" & PI %in% c("O", "N"))
maa1df[] <- lapply(maa1df, function(x) if(is.factor(x)) factor(x) else x)


#Sum contrast coding of the relevant variables
contrasts(maa1df$PI) <- contr.sum(2)
contrasts(maa1df$Quantifier) <- contr.sum(2)
contrasts(maa1df$Increasing) <- contr.sum(2)

#contrast coding: N:1, 0:-1
maa1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = maa1df,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(maa1)


# Probability that the presence of PPI increases the judgments
post_samples_maa1 = posterior_samples(maa1)
mean(post_samples_maa1$b_PI1 < 0)

# Summary of the prior
prior_summary(maa1)

######################
###PPI in NM####
######################
mab1df <- subset(AnDataSet, Monotonicity == "NM" & PI %in% c("O", "P"))
mab1df[] <- lapply(mab1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
mab1df$PI <- relevel(mab1df$PI, "P")
contrasts(mab1df$PI) <- contr.sum(2)
contrasts(mab1df$Quantifier) <- contr.sum(2)
contrasts(mab1df$Increasing) <- contr.sum(2)

#NB: In Exp 4, we cannot have by-subject slopes for PIs (half of participant never sees an NPI in a NM environment, and half of participants never sees a PPI in an NM environment, so PI slopes could at best be estimated for half of participants)
#contrast coding: P:1, 0:-1
mab1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = mab1df,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(mab1)


# Probability that the presence of PPI increases the judgments
post_samples_mab1 = posterior_samples(mab1)
mean(post_samples_mab1$b_PI1 > 0)

# Summary of the prior
prior_summary(mab1)

######################
###NPI in DD####
######################

mac1df <- subset(AnDataSet, Monotonicity == "DD" & PI %in% c("O", "N"))
mac1df[] <- lapply(mac1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(mac1df$PI) <- contr.sum(2)
contrasts(mac1df$Quantifier) <- contr.sum(2)
contrasts(mac1df$Increasing) <- contr.sum(2)

#contrast coding: N:1, 0:-1
mac1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = mac1df,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(mac1)


# Probability that the presence of NPI lowers the judgments
post_samples_mac1 = posterior_samples(mac1)
mean(post_samples_mac1$b_PI1 < 0)

# Summary of the prior
prior_summary(mac1)

######################
###PPI in DD####
######################
mad1df <- subset(AnDataSet, Monotonicity == "DD" & PI %in% c("O", "P"))
mad1df[] <- lapply(mad1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
mad1df$PI <- relevel(mad1df$PI, "P")
contrasts(mad1df$PI) <- contr.sum(2)
contrasts(mad1df$Quantifier) <- contr.sum(2)
contrasts(mad1df$Increasing) <- contr.sum(2)

#contrast coding: P:1, 0:-1
mad1 = brm(
  formula = Irep ~ PI + Quantifier + Increasing+ (1+PI + Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = mad1df,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(mad1)

# Probability that the presence of PPI increases the judgments
post_samples_mad1 = posterior_samples(mad1)
mean(post_samples_mad1$b_PI1 > 0)

# Summary of the prior
prior_summary(mad1)


############################################################
##META-ANALYSIS ON UPWARD AND DOWNWARD INFERENCES SEPARATELY
############################################################
###NPI in NM D####
dema1df <- subset(AnDataSet,  Monotonicity == "NM" & Increasing =="D" & PI %in% c("O", "N"))
dema1df[] <- lapply(dema1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(dema1df$PI) <- contr.sum(2)
contrasts(dema1df$Quantifier) <- contr.sum(2)

#contrast coding: N:1, 0:-1
dema1 = brm(
  formula = Ireponse ~ PI + Quantifier + (1+PI + Quantifier | Content)+ (1+ Quantifier| subject),
  data = dema1df,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(dema1)


# Probability that the presence of NPI boosts raw DE judgments
post_samples_dema1 = posterior_samples(dema1)
mean(post_samples_dema1$b_PI1 > 0)

# Summary of the prior
prior_summary(dema1)

###NPI in NM U####
#Models with maximal random effects structure don't converge.
uema1df <- subset(AnDataSet,  Monotonicity == "NM" & Increasing =="U" & PI %in% c("O", "N"))
uema1df[] <- lapply(uema1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(uema1df$PI) <- contr.sum(2)
contrasts(uema1df$Quantifier) <- contr.sum(2)

#contrast coding: N:1, 0:-1
uema1 = brm(
  formula = Ireponse ~ PI + Quantifier + (1+PI + Quantifier | Content)+ (1+ Quantifier| subject),
  data = uema1df,
  iter = 8000, save_pars = save_pars(all = TRUE))
summary(uema1)

# Probability that the presence of NPI decreases the U judgments
post_samples_uema1 = posterior_samples(uema1)
mean(post_samples_uema1$b_PI1 < 0)

# Summary of the prior
prior_summary(uema1)

###PPI in NM D####

demb1df <- subset(AnDataSet,  Monotonicity == "NM" & Increasing =="D" & PI %in% c("O", "P")) 
demb1df[] <- lapply(demb1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
demb1df$PI <- relevel(demb1df$PI, "P")
contrasts(demb1df$PI) <- contr.sum(2)
contrasts(demb1df$Quantifier) <- contr.sum(2)

#contrast coding: P:1, 0:-1
demb1 = brm(
  formula = Ireponse ~ PI + Quantifier + (1+PI + Quantifier | Content)+ (1+ Quantifier| subject),
  data = demb1df,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(demb1)

# Probability that the presence of PPI decreases the judgments
post_samples_demb1 = posterior_samples(demb1)
mean(post_samples_demb1$b_PI1 < 0)

# Summary of the prior
prior_summary(demb1)

###PPI in NM U####

uemb1df <- subset(AnDataSet,  Monotonicity == "NM" & Increasing =="U" & PI %in% c("O", "P"))
uemb1df[] <- lapply(uemb1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
uemb1df$PI <- relevel(uemb1df$PI, "P")
contrasts(uemb1df$PI) <- contr.sum(2)
contrasts(uemb1df$Quantifier) <- contr.sum(2)

#contrast coding: P:1, 0:-1
uemb1 = brm(
  formula = Ireponse ~ PI + Quantifier + (1+PI + Quantifier | Content)+ (1+ Quantifier| subject),
  data = uemb1df,
  iter = 10000, save_pars = save_pars(all = TRUE))
summary(uemb1)

# Probability that the presence of PPI increases the judgments
post_samples_uemb1 = posterior_samples(uemb1)
mean(post_samples_uemb1$b_PI1 > 0)

# Summary of the prior
prior_summary(uemb1)

###NPI in DD D####
demc1df <- subset(AnDataSet,  Monotonicity == "DD" & Increasing =="D" & PI %in% c("O", "N"))
demc1df[] <- lapply(demc1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(demc1df$PI) <- contr.sum(2)
contrasts(demc1df$Quantifier) <- contr.sum(2)

#Full model
#contrast coding: N:1, 0:-1
demc1 = brm(
  formula = Ireponse ~ PI + Quantifier + (1+PI + Quantifier | Content)+ (1+ Quantifier| subject),
  data = demc1df,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(demc1)


# Probability that the presence of NPI increases the judgments
post_samples_demc1 = posterior_samples(demc1)
mean(post_samples_demc1$b_PI1 > 0)

# Summary of the prior
prior_summary(demc1)


###NPI in DD U####

uemc1df <-subset(AnDataSet,  Monotonicity == "DD" & Increasing =="U" & PI %in% c("O", "N"))
uemc1df[] <- lapply(uemc1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(uemc1df$PI) <- contr.sum(2)
contrasts(uemc1df$Quantifier) <- contr.sum(2)

#Full model
#contrast coding: N:1, 0:-1

uemc1 = brm(
  formula = Ireponse ~ PI + Quantifier + (1+PI + Quantifier | Content)+ (1+ Quantifier| subject),
  data = uemc1df,
  iter = 8000, save_pars = save_pars(all = TRUE))
summary(uemc1)


# Probability that the presence of NPI decreases the judgments
post_samples_uemc1 = posterior_samples(uemc1)
mean(post_samples_uemc1$b_PI1 < 0)

# Summary of the prior
prior_summary(uemc1)


###PPI in DD D####
demd1df <- subset(AnDataSet,  Monotonicity == "DD" & Increasing =="D" & PI %in% c("O", "P"))
demd1df[] <- lapply(demd1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
demd1df$PI <- relevel(demd1df$PI, "P")
contrasts(demd1df$PI) <- contr.sum(2)
contrasts(demd1df$Quantifier) <- contr.sum(2)

#Full model
#contrast coding: P:1, 0:-1

demd1 = brm(
  formula = Ireponse ~ PI + Quantifier + (1+PI + Quantifier | Content)+ (1+ Quantifier| subject),
  data = demd1df,
  iter = 8000, save_pars = save_pars(all = TRUE))
summary(demd1)


# Probability that the presence of PPI decreases the judgments
post_samples_demd1 = posterior_samples(demd1)
mean(post_samples_demd1$b_PI1 < 0)

# Summary of the prior
prior_summary(demd1)

###PPI in DD U####
uemd1df <- subset(AnDataSet,  Monotonicity == "DD" & Increasing =="U" & PI %in% c("O", "P"))
uemd1df[] <- lapply(uemd1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
uemd1df$PI <- relevel(uemd1df$PI, "P")
contrasts(uemd1df$PI) <- contr.sum(2)
contrasts(uemd1df$Quantifier) <- contr.sum(2)


#Full model
#contrast coding: P:1, 0:-1

uemd1 = brm(
  formula = Ireponse ~ PI + Quantifier + (1+PI + Quantifier | Content)+ (1+ Quantifier| subject),
  data = uemd1df,
  iter = 8000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(uemd1)

# Probability that the presence of PPI increases the judgments
post_samples_uemd1 = posterior_samples(uemd1)
mean(post_samples_uemd1$b_PI1 > 0)

# Summary of the prior
prior_summary(uemd1)

##########################################
#Looking at environment-PI interactions (is it NM only, or DE too)
#Most interactions are significant, so we can indeed make the case that PIs influence more in uncertain environments.
###########################################
expts = c("NM", "NMCons", "DD", "DDSplit")
envts = c("NM", "DD", "DE", "UE")
PIs = c("O", "N", "P")

FullDataSet <- subset(DDDD, EXP %in% expts & Monotonicity %in% envts & PI %in% PIs)
FullDataSet$subject <- as.factor(FullDataSet$subject)
FullDataSet$EXP <- as.factor(FullDataSet$EXP)

### NPI in DE vs. NM
envpia1df <-subset(FullDataSet, Monotonicity %in% c("NM", "DE") & PI %in% c("O", "N"))
envpia1df[] <- lapply(envpia1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(envpia1df$PI) <- contr.sum(2)
contrasts(envpia1df$Quantifier) <- contr.sum(5)
contrasts(envpia1df$Monotonicity) <- contr.sum(2)
contrasts(envpia1df$Increasing) <- contr.sum(2)

#Full model
#DE 1, NPI 1
envpia1 = brm(
  formula = Irep ~ PI*Monotonicity  + Increasing + (1|Quantifier) + (1+ PI*Monotonicity  + Increasing| Content)+ (1+ Monotonicity + Increasing| subject),
  data = envpia1df,
  iter = 2000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.999, max_treedepth = 15))
summary(envpia1)


#Probability that the PI has a larger effect in one environment
post_samples_envpia1 = posterior_samples(envpia1)
mean(post_samples_envpia1$`b_PI1:Monotonicity1` > 0)

# Summary of the prior
prior_summary(envpia1)


###NPI in DN vs. DE

envpib1df <-subset(FullDataSet, Monotonicity %in% c("DD", "DE") & PI %in% c("O", "N"))
envpib1df[] <- lapply(envpib1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(envpib1df$PI) <- contr.sum(2)
contrasts(envpib1df$Quantifier) <- contr.sum(5)
contrasts(envpib1df$Monotonicity) <- contr.sum(2)
contrasts(envpib1df$Increasing) <- contr.sum(2)

envpib1 = brm(
  formula = Irep ~ PI*Monotonicity  + Increasing + (1|Quantifier) + (1+ PI*Monotonicity  + Increasing| Content)+ (1+ Monotonicity + Increasing| subject),
  data = envpib1df,
  iter = 4000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(envpib1)


#Probability that the PI has a larger effect in one environment
post_samples_envpib1 = posterior_samples(envpib1)
mean(post_samples_envpib1$`b_PI1:Monotonicity1` < 0)

# Summary of the prior
prior_summary(envpib1)


###PPI in NM vs. UE
envpic1df <-subset(FullDataSet, Monotonicity %in% c("NM", "UE") & PI %in% c("O", "P"))
envpic1df[] <- lapply(envpic1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
envpic1df$PI <- relevel(envpic1df$PI, "P")
contrasts(envpic1df$PI) <- contr.sum(2)
contrasts(envpic1df$Quantifier) <- contr.sum(5)
contrasts(envpic1df$Monotonicity) <- contr.sum(2)
contrasts(envpic1df$Increasing) <- contr.sum(2)

envpic1 = brm(
  formula = Irep ~ PI*Monotonicity  + Increasing + (1|Quantifier) + (1+ PI*Monotonicity  + Increasing| Content)+ (1+ Monotonicity + Increasing| subject),
  data = envpic1df,
  iter = 4000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(envpic1)


#Probability that the PI has a larger effect in one environment
post_samples_envpic1 = posterior_samples(envpic1)
mean(post_samples_envpic1$`b_PI1:Monotonicity1` > 0)

# Summary of the prior
prior_summary(envpic1)


###PPIs in DD vs. UE

envpid1df <-subset(FullDataSet, Monotonicity %in% c("DD", "UE") & PI %in% c("O", "P"))
envpid1df[] <- lapply(envpid1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
envpid1df$PI <- relevel(envpid1df$PI, "P")
contrasts(envpid1df$PI) <- contr.sum(2)
contrasts(envpid1df$Quantifier) <- contr.sum(5)
contrasts(envpid1df$Monotonicity) <- contr.sum(2)
contrasts(envpid1df$Increasing) <- contr.sum(2)

envpid1 = brm(
  formula = Irep ~ PI*Monotonicity  + Increasing + (1|Quantifier) + (1+ PI*Monotonicity  + Increasing| Content)+ (1+ Monotonicity + Increasing| subject),
  data = envpid1df,
  iter = 8000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.999, max_treedepth = 15))
summary(envpid1)



#Probability that the PI has a larger effect in one environment
post_samples_envpid1 = posterior_samples(envpid1)
mean(post_samples_envpid1$`b_PI1:Monotonicity1` > 0)

# Summary of the prior
prior_summary(envpid1)

###NPIs in DD vs. NM
envpie1df <-subset(FullDataSet, Monotonicity %in% c("DD", "NM") & PI %in% c("O", "N"))
envpie1df[] <- lapply(envpie1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
contrasts(envpie1df$PI) <- contr.sum(2)
contrasts(envpie1df$Quantifier) <- contr.sum(4)
contrasts(envpie1df$Monotonicity) <- contr.sum(2)
contrasts(envpie1df$Increasing) <- contr.sum(2)


envpie1 = brm(
  formula = Irep ~ PI*Monotonicity  + Increasing + (1|Quantifier) + (1+ PI*Monotonicity  + Increasing| Content)+ (1+ Monotonicity + Increasing| subject),
  data = envpie1df,
  iter = 8000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(envpie1)


#Probability that the PI has a larger effect in one environment
post_samples_envpie1 = posterior_samples(envpie1)
mean(post_samples_envpie1$`b_PI1:Monotonicity1` < 0)

# Summary of the prior
prior_summary(envpie1)



###PPIs in DD vs. NM
envpif1df <-subset(FullDataSet, Monotonicity %in% c("DD", "NM") & PI %in% c("O", "P"))
envpif1df[] <- lapply(envpif1df, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
envpif1df$PI <- relevel(envpif1df$PI, "P")
contrasts(envpif1df$PI) <- contr.sum(2)
contrasts(envpif1df$Quantifier) <- contr.sum(4)
contrasts(envpif1df$Monotonicity) <- contr.sum(2)
contrasts(envpif1df$Increasing) <- contr.sum(2)

envpif1 = brm(
  formula = Irep ~ PI*Monotonicity  + Increasing + (1|Quantifier) + (1+ PI*Monotonicity  + Increasing| Content)+ (1+ Monotonicity + Increasing| subject),
  data = envpif1df,
  iter = 8000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.999, max_treedepth = 15))
summary(envpif1)

#Probability that the PI has a larger effect in one environment
post_samples_envpif1 = posterior_samples(envpif1)
mean(post_samples_envpif1$`b_PI1:Monotonicity1` > 0)

# Summary of the prior
prior_summary(envpif1)

###########################################
#Appendix A: effect of monotonicity on inferential judgments
###########################################
expts = c("NM", "NMCons", "DD", "DDSplit")
envts = c("NM", "DD", "DE", "UE")
PIs = c("O", "N", "P")

FullDataSet <- subset(DDDD, EXP %in% expts & Monotonicity %in% envts & PI %in% PIs)
FullDataSet$subject <- as.factor(FullDataSet$subject)
FullDataSet$EXP <- as.factor(FullDataSet$EXP)

##############
### DE ratings
##############
#### NM vs UE ####
envinfadf <-subset(FullDataSet, Monotonicity %in% c("NM", "UE") & Increasing %in% c("D"))
envinfadf[] <- lapply(envinfadf, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
envinfadf$Monotonicity = factor(envinfadf$Monotonicity)
contrasts(envinfadf$Monotonicity) <- contr.sum(2)
contrasts(envinfadf$Monotonicity)

envinfa1 = brm(
  formula = Ireponse ~  Monotonicity + (1+ Monotonicity| subject),
  data = envinfadf,
  iter = 4000, save_pars = save_pars(all = TRUE))
summary(envinfa1)

post_samples_envinfa1 = posterior_samples(envinfa1)
mean(post_samples_envinfa1$`b_Monotonicity1` < 0)


#### DD vs UE ####
envinfbdf <-subset(FullDataSet, Monotonicity %in% c("DD", "UE") & Increasing %in% c("D"))
envinfbdf[] <- lapply(envinfbdf, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
envinfbdf$Monotonicity<- relevel(envinfbdf$Monotonicity, "UE")
envinfbdf$Monotonicity = factor(envinfbdf$Monotonicity)
contrasts(envinfbdf$Monotonicity) <- contr.sum(2)
contrasts(envinfbdf$Monotonicity)

envinfb1 = brm(
  formula = Ireponse ~  Monotonicity + (1+ Monotonicity| subject),
  data = envinfbdf,
  iter = 4000, save_pars = save_pars(all = TRUE))
summary(envinfb1)

post_samples_envinfb1 = posterior_samples(envinfb1)
mean(post_samples_envinfb1$`b_Monotonicity1` < 0)

#### DD vs DE ####
envinfcdf <-subset(FullDataSet, Monotonicity %in% c("DD", "DE") & Increasing %in% c("D"))
envinfcdf[] <- lapply(envinfcdf, function(x) if(is.factor(x)) factor(x) else x)

envinfcdf$Monotonicity = factor(envinfcdf$Monotonicity)
contrasts(envinfcdf$Monotonicity) <- contr.sum(2)
contrasts(envinfcdf$Monotonicity)

envinfc1 = brm(
  formula = Ireponse ~  Monotonicity + (1+ Monotonicity| subject),
  data = envinfcdf,
  iter = 4000, save_pars = save_pars(all = TRUE))
summary(envinfc1)

post_samples_envinfc1 = posterior_samples(envinfc1)
mean(post_samples_envinfc1$`b_Monotonicity1` < 0)


##############
### UE ratings
##############
#### NM vs DE ####
envinfddf <-subset(FullDataSet, Monotonicity %in% c("NM", "DE") & Increasing %in% c("U"))
envinfddf[] <- lapply(envinfddf, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
envinfddf$Monotonicity = factor(envinfddf$Monotonicity)
contrasts(envinfddf$Monotonicity) <- contr.sum(2)
contrasts(envinfddf$Monotonicity)

envinfd1 = brm(
  formula = Ireponse ~  Monotonicity + (1+ Monotonicity| subject),
  data = envinfddf,
  iter = 4000, save_pars = save_pars(all = TRUE))
summary(envinfd1)

post_samples_envinfd1 = posterior_samples(envinfd1)
mean(post_samples_envinfd1$`b_Monotonicity1` < 0)

#### NM vs DD ####
envinfedf <-subset(FullDataSet, Monotonicity %in% c("NM", "DD") & Increasing %in% c("U"))
envinfedf[] <- lapply(envinfedf, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
envinfedf$Monotonicity = factor(envinfedf$Monotonicity)
contrasts(envinfedf$Monotonicity) <- contr.sum(2)
contrasts(envinfedf$Monotonicity)

envinfe1 = brm(
  formula = Ireponse ~  Monotonicity + (1+ Monotonicity| subject),
  data = envinfedf,
  iter = 4000, save_pars = save_pars(all = TRUE))
summary(envinfe1)

post_samples_envinfe1 = posterior_samples(envinfe1)
mean(post_samples_envinfe1$`b_Monotonicity1` < 0)

#### DD vs UE ####
envinffdf <-subset(FullDataSet, Monotonicity %in% c("UE", "DD") & Increasing %in% c("U"))
envinffdf[] <- lapply(envinffdf, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
envinffdf$Monotonicity = factor(envinffdf$Monotonicity)
contrasts(envinffdf$Monotonicity) <- contr.sum(2)
contrasts(envinffdf$Monotonicity)

envinff1 = brm(
  formula = Ireponse ~  Monotonicity + (1+ Monotonicity| subject),
  data = envinffdf,
  iter = 4000, save_pars = save_pars(all = TRUE))
summary(envinff1)

post_samples_envinff1 = posterior_samples(envinff1)
mean(post_samples_envinff1$`b_Monotonicity1` < 0)

###########################################
#Appendix B: Quantifier-PI interaction
###########################################

expts = c("NM", "NMCons", "DD", "DDSplit")
envts = c("NM", "DD")

PIs = c("O", "N", "P")

AnDataSet <- subset(DDDD, EXP %in% expts & Monotonicity %in% envts & PI %in% PIs)
AnDataSet$subject <- as.factor(AnDataSet$subject)
AnDataSet$EXP <- as.factor(AnDataSet$EXP)


#NPI in NM
maa1appbdf <- subset(AnDataSet, Monotonicity == "NM" & PI %in% c("O", "N"))
maa1appbdf[] <- lapply(maa1appbdf, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
maa1appbdf$PI = factor(maa1appbdf$PI)
maa1appbdf$Quantifier = factor(maa1appbdf$Quantifier)
maa1appbdf$Increasing = factor(maa1appbdf$Increasing)

contrasts(maa1appbdf$PI) <- contr.sum(2)
contrasts(maa1appbdf$Quantifier) <- contr.sum(2)
contrasts(maa1appbdf$Increasing) <- contr.sum(2)

maa1appb = brm(
  formula = Irep ~ PI*Quantifier + Increasing+ (1+PI*Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = maa1appbdf,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(maa1appb)

# Probability that the presence of PPI increases the judgments
post_samples_maa1appb = posterior_samples(maa1appb)
mean(post_samples_maa1appb$`b_PI1:Quantifier1` < 0)

#NPI in DN

mac1appbdf <- subset(AnDataSet, Monotonicity == "DD" & PI %in% c("O", "N"))
mac1appbdf[] <- lapply(mac1appbdf, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
mac1appbdf$PI = factor(mac1appbdf$PI)
mac1appbdf$Quantifier = factor(mac1appbdf$Quantifier)
mac1appbdf$Increasing = factor(mac1appbdf$Increasing)

contrasts(mac1appbdf$PI) <- contr.sum(2)
contrasts(mac1appbdf$Quantifier) <- contr.sum(2)
contrasts(mac1appbdf$Increasing) <- contr.sum(2)

mac1appb = brm(
  formula = Irep ~ PI*Quantifier + Increasing+ (1+PI*Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = mac1appbdf,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(mac1appb)

# Probability that the presence of PPI increases the judgments
post_samples_mac1appb = posterior_samples(mac1appb)
mean(post_samples_mac1appb$`b_PI1:Quantifier1` < 0)

#PPI in NM
mab1appbdf <- subset(AnDataSet, Monotonicity == "NM" & PI %in% c("O", "P"))
mab1appbdf[] <- lapply(mab1appbdf, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
mab1appbdf$PI = factor(mab1appbdf$PI)
mab1appbdf$Quantifier = factor(mab1appbdf$Quantifier)
mab1appbdf$Increasing = factor(mab1appbdf$Increasing)

mab1appbdf$PI <- relevel(mab1appbdf$PI, "P")
contrasts(mab1appbdf$PI) <- contr.sum(2)
contrasts(mab1appbdf$Quantifier) <- contr.sum(2)
contrasts(mab1appbdf$Increasing) <- contr.sum(2)

mab1appb = brm(
  formula = Irep ~ PI*Quantifier + Increasing+ (1+PI*Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = mab1appbdf,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(mab1appb)

# Probability that the presence of PPI increases the judgments
post_samples_mab1appb = posterior_samples(mab1appb)
mean(post_samples_mab1appb$`b_PI1:Quantifier1` > 0)

#PPI in DD
mad1appbdf <- subset(AnDataSet, Monotonicity == "DD" & PI %in% c("O", "P"))
mad1appbdf[] <- lapply(mad1appbdf, function(x) if(is.factor(x)) factor(x) else x)

#Sum contrast coding of the relevant variables
mad1appbdf$PI <- relevel(mad1appbdf$PI, "P")
mad1appbdf$PI = factor(mad1appbdf$PI)
mad1appbdf$Quantifier = factor(mad1appbdf$Quantifier)
mad1appbdf$Increasing = factor(mad1appbdf$Increasing)

contrasts(mad1appbdf$PI) <- contr.sum(2)
contrasts(mad1appbdf$Quantifier) <- contr.sum(2)
contrasts(mad1appbdf$Increasing) <- contr.sum(2)

contrasts(mad1appbdf$PI) 
contrasts(mad1appbdf$Quantifier)
contrasts(mad1appbdf$Increasing) 

mad1appb = brm(
  formula = Irep ~ PI*Quantifier + Increasing+ (1+PI*Quantifier + Increasing| Content)+ (1+ Increasing+ Quantifier| subject),
  data = mad1appbdf,
  iter = 10000, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
summary(mad1appb)

# Probability that the presence of PPI increases the judgments
post_samples_mad1appb = posterior_samples(mad1appb)
mean(post_samples_mad1appb$`b_PI1:Quantifier1` > 0)



