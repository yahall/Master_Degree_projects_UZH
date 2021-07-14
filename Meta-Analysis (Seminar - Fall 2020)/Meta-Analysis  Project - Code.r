### Getting started ####
# Set working directory
setwd("~/Yannik/UZH/20HS/Meta-Analysis (S)/Final Project/R")

# Load packages
# Packages from the system library (do not have to be installed first)
pkg_sys <- c("datasets", "foreign", "MASS", "stats","stats4","nlme","grid")

# Additional packages (need to be installed prior to loading)
pkg <- c("dplyr","tidyr","ggplot2","stargazer","reshape2","readr","haven","dummies",
         "Hmisc","lmtest","sandwich","doBy","readxl","multiwayvcov","miceadds","car",
         "purrr","knitr","wesanderson","ggvis","lubridate","reporttools", 
         "stringr", "data.table", "devtools","tinytex","rmarkdown","matlib",
         "ggiraphExtra", "gridExtra","meta","kableExtra","table1")

# lapply(pkg, install.packages, character.only = FALSE)  # install packages if not already done
invisible(lapply(c(pkg, pkg_sys), library, character.only = TRUE))
rm(list=ls())



### Read in & reshape the data ####
EE_DP_cor <- read.csv("~/Yannik/UZH/20HS/Meta-Analysis (S)/Final Project/R/EE_DP_corr_data.csv", sep = ";")
EE_DP_cor <- as.data.frame(EE_DP_cor)

# Create a column containing an unbiased approximation of the population correlation coefficient G(r)
# See DeCoster Eq. (5.2) p.16
EE_DP_cor$G_cor <- EE_DP_cor$cor + ((EE_DP_cor$cor*(1-(EE_DP_cor$cor^2)))/
                                      (2*(EE_DP_cor$n-3)))

# Take a look at the data and reshape it as desired
str(EE_DP_cor)
EE_DP_cor[EE_DP_cor == "N/A"] <- NA
EE_DP_cor <- as.data.table(EE_DP_cor)
setkey(EE_DP_cor, G_cor)
View(EE_DP_cor)

# Identify the studies with pearson correlation
EE_DP_cor$study_nr <- c(1:nrow(EE_DP_cor))
include <- EE_DP_cor$study_nr[EE_DP_cor$cor_type == "pearson"]
EE_DP_use <- EE_DP_cor[include]
EE_DP_cor$study_nr <- NULL
EE_DP_use$study_nr <- NULL

# Drop unused variables
EE_DP_use$MBI_type                  <- NULL
EE_DP_use$cor_type                  <- NULL
EE_DP_use$country                   <- NULL
EE_DP_use$stable_relationship_share <- NULL

# Clean dataset
str(EE_DP_use)
EE_DP_use$region           <- as.factor(EE_DP_use$region)
EE_DP_use$facility_type    <- as.factor(EE_DP_use$facility_type)
EE_DP_use$experience_mean  <- as.numeric(EE_DP_use$experience_mean)
EE_DP_use$age_mean         <- as.numeric(EE_DP_use$age_mean )
EE_DP_use$female_share     <- as.numeric(EE_DP_use$female_share)
EE_DP_use$BA_deg_up_share  <- as.numeric(EE_DP_use$BA_deg_up_share)
str(EE_DP_use)



### Explanatory Data Analysis ####
# Create an overview table (i.e. Table 1)
Overview <- EE_DP_use[, c("study_label","n","cor")]
colnames(Overview) <- c("Study Label","Sample size","Pearson correlation")
View(Overview)

# Create a latex table using kableExtra
Overview_k <- kable(Overview, format = 'latex', digits = 3, booktabs = T, 
                    linesep = "", align=c("l","r","c"))
Overview_k <- kable_styling(Overview_k, latex_options = c("striped"))
Overview_k

# Creating table of summary statistics 
stargazer(EE_DP_use, out = "./sumstats.html", 
          title = "Summary Statistics",
          digits = 2 , 
          omit = "G_cor", 
          omit.summary.stat = c("p25","p75"),
          summary.stat = c("n","mean", "sd", "min", "median", "max"),
          covariate.labels = c("PCEEDP","sample size","mean experience", "mean age",
                               "share female","share BA degree or higher"))

# Creating correlation matrix
EE_DP_use_cor <- as.matrix(cbind(EE_DP_use$cor, EE_DP_use$n,
                                 EE_DP_use$experience_mean,
                                 EE_DP_use$age_mean,
                                 EE_DP_use$female_share,
                                 EE_DP_use$BA_deg_up_share))

cormatrix <- round(cor(EE_DP_use_cor, use = "pairwise.complete.obs"), 2)
upper <- cormatrix
upper[upper.tri(cormatrix)] <- ""
upper <- as.data.frame(upper)
colnames(upper) <- c("1", "2","3","4","5","6" )

stargazer(upper, summary=FALSE, rownames=F, out="./upper.html", title = "Table of Correlations")

# Note: the tables "sumstats.html" and "upper.html" are put side-by-side to 
# obtain figure "Summary Statistics" (i.e. Table 2)

# Create scatter plots for each continuous moderator
# Mean Experience
expplot <- ggplot(EE_DP_use[!is.na(EE_DP_use$experience_mean),], 
                aes(experience_mean,cor)) +
  geom_point() +
  xlab("Mean Experience") +
  ylab("PCEEDP") +
  theme_bw() +
  theme(legend.position = "none")


# Mean Age
ageplot <- ggplot(EE_DP_use[!is.na(EE_DP_use$age_mean),], 
                aes(age_mean,cor)) +
  geom_point() +
  xlab("Mean Age") +
  ylab("PCEEDP") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Female Share
femplot <- ggplot(EE_DP_use[!is.na(EE_DP_use$female_share),], 
                aes(female_share,cor)) +
  geom_point() +
  xlab("Female Share") +
  ylab("PCEEDP") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Share of individuals with Bachelor degree or higher
BAplot <- ggplot(EE_DP_use[!is.na(EE_DP_use$BA_deg_up_share),], 
               aes(BA_deg_up_share,cor)) +
  geom_point() +
  xlab("Bachelor's Degree or higher Share") +
  ylab("PCEEDP") +
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Get scatter plots side by side to create Figure 1
grid.newpage()
grid.draw(cbind(ggplotGrob(expplot), ggplotGrob(ageplot), ggplotGrob(femplot),ggplotGrob(BAplot), size = "last"))

# Summery table by region to see if there is large differences based on region in variables
table1::label(EE_DP_use$cor) <- "PCEEDP"
table1::label(EE_DP_use$experience_mean) <- "Mean Experience"
table1::label(EE_DP_use$age_mean) <- "Mean Age"
table1::label(EE_DP_use$female_share) <- "Share Female"
table1::label(EE_DP_use$BA_deg_up_share) <- "Share BA degree or higher"
table1::label(EE_DP_use$n) <- "Sample Size"

rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(render.categorical.default(x))
  what <- switch(name,
                 cor = "Mean (SD)",
                 n= "Mean (SD)",
                 experience_mean  = "Mean (SD)",
                 age_mean = "Mean (SD)",
                 female_share= "Mean (SD)",
                 BA_deg_up_share = "Mean (SD)")
  
  parse.abbrev.render.code(c("", what))(x)
}

table1(~ cor+n+experience_mean+age_mean+female_share+BA_deg_up_share| facility_type, data=EE_DP_use, overall = "All Facilities", render = rndr, output = "html")

table1(~ cor+n+experience_mean+age_mean+female_share+BA_deg_up_share| region, data=EE_DP_use, overall = "All Regions", render = rndr, output = "html")



### Meta analysis ####
# Create the meta object
meta_obj <- metacor(G_cor, n, study_label, EE_DP_use)

# Take a look at the summary
print(summary(meta_obj), digits = 2)

# Create a forest plot (i.e. Figure 2)
forest(meta_obj)

# Create a funnel plot (i.e. Figure 4)
funnel(meta_obj,studlab = T,cex.studlab = 0.5, pos.studlab = 4, ref.triangle = T)



### Moderator analysis ####
# Add a column containing Fisher's z value of G(r)
EE_DP_use$fishers_z <- meta_obj$TE

# Get the weights assigned to each study in our preferred model (i.e. the random effect model)
EE_DP_use$weight <- meta_obj$w.random

## To get a rough idea of the relationship between the continuous moderators of interest and the
## dependent variable we create a scatter-plot for each continuous moderators (i.e. Figure 3)

grid.arrange(
# Mean Experience
ggplot(EE_DP_use[!is.na(EE_DP_use$experience_mean),], 
       aes(experience_mean,fishers_z)) +
  geom_point() +
  xlab("Mean Experience") +
  ylab("Fisher's z") +
  ylim(0.4,0.9) +
  theme_bw() +
  geom_smooth(method = 'lm', formula = y~x, 
              alpha = 0, col = 'red', size = 0.5),

# Mean Age
ggplot(EE_DP_use[!is.na(EE_DP_use$age_mean),], 
       aes(age_mean,fishers_z)) +
  geom_point() +
  xlab("Mean Age") +
  ylab("Fisher's z") +
  ylim(0.4,0.9) +
  theme_bw() +
  geom_smooth(method = 'lm', formula = y~x, 
              alpha = 0, col = 'red', size = 0.5),

# Female Share
ggplot(EE_DP_use[!is.na(EE_DP_use$female_share),], 
       aes(female_share,fishers_z)) +
  geom_point() +
  xlab("Female Share") +
  ylab("Fisher's z") +
  ylim(0.4,0.9) +
  theme_bw() +
  geom_smooth(method = 'lm', formula = y~x, 
              alpha = 0, col = 'red', size = 0.5),

# Share of individuals with Bachelor degree or higher
ggplot(EE_DP_use[!is.na(EE_DP_use$BA_deg_up_share),], 
       aes(BA_deg_up_share,fishers_z)) +
  geom_point() +
  xlab("Share Bachelor Degree Up") +
  ylab("Fisher's z") +
  ylim(0.4,0.9) +
  theme_bw() +
  geom_smooth(method = 'lm', formula = y~x, 
              alpha = 0, col = 'red', size = 0.5),
ncol = 4)

## Note: Analyzing these plots suggests that for all variables a common weighted linear regression
## model seems to be adequate.

## Then we prepare our data for the moderator analysis

# Normalize all continuous moderators before running the regressions
EE_DP_use$experience_mean <- as.numeric(scale(EE_DP_use$experience_mean))
EE_DP_use$age_mean        <- as.numeric(scale(EE_DP_use$age_mean))
EE_DP_use$female_share    <- as.numeric(scale(EE_DP_use$female_share))
EE_DP_use$BA_deg_up_share <- as.numeric(scale(EE_DP_use$BA_deg_up_share))


# Run a weighted linear regression for each moderator of interest against the normalized 
# Fishers-transformed correlations to identify potentially relevant moderators
mod_reg0 <- lm(fishers_z ~ 0 + region, 
              data = EE_DP_use,
              weights = EE_DP_use$weight)

mod_reg1 <- lm(fishers_z ~ 1 + region, 
               data = EE_DP_use,
               weights = EE_DP_use$weight)

mod_fac0 <- lm(fishers_z ~ 0 + facility_type, 
              data = EE_DP_use,
              weights = EE_DP_use$weight)

mod_fac1 <- lm(fishers_z ~ 1 + facility_type, 
              data = EE_DP_use,
              weights = EE_DP_use$weight)

mod_exp <- lm(fishers_z ~ 1 + experience_mean, 
              data = EE_DP_use,
              weights = EE_DP_use$weight)

mod_age <- lm(fishers_z ~ 1 + age_mean, 
              data = EE_DP_use,
              weights = EE_DP_use$weight)

mod_fem <- lm(fishers_z ~ 1 + female_share, 
              data = EE_DP_use,
              weights = EE_DP_use$weight)

mod_BAd <- lm(fishers_z ~ 1 + BA_deg_up_share, 
              data = EE_DP_use,
              weights = EE_DP_use$weight)

# Take a look at the results from the simple regression models
# Discrete moderators (i.e. Table 3)
stargazer(mod_reg1,mod_reg0,mod_fac1,mod_fac0, 
          type = "latex",title = "Factorial Moderators", report="vc*p",
          omit.stat = c("f","ser"))

# Continuous moderators (i.e. Table 4)
stargazer(mod_exp,mod_age,mod_fem,mod_BAd,
          type = "latex", title = "Continuous Moderators", report="vc*p",
          omit.stat = c("f","ser"))


## Then we continue by taking the most valuable moderators and combine them in a multiple
## regression model
mod_mult <- lm(fishers_z ~ 1 + experience_mean + 
               age_mean + female_share, data = EE_DP_use,
               weights = EE_DP_use$weight)

# Take a look at the results from the multiple regression models (i.e. Table 5)
stargazer(mod_mult, type = "latex", report="vc*p")
