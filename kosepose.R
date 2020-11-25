# By Kajsa M{\o}llersen (kajsa.mollersen@uit.no)

# Investigating whether kosepose is different from conventional trawl. 
# All data are provided by Tonje Jensen (tonje.k.jensen@uit.no).

## Library ####################################################

library(readxl) # to read the excel sheet
library(tibble) # to create tibble instead of data.frame
library(dplyr) # to drop column from tibble, using select()

## Read data ####################################################

# I first read the Excel-sheet provided by Tonje and have a look at it. 
Workfile <- read_excel("/Volumes/kam025/Dokaments/Kosepose/Bleeding time and bleeding method.xlsx") 

## Choose a part of the fish #################################################

# args = commandArgs(trailingOnly = TRUE) 
# resp_var_name = args[1]
resp_var_name = c("Tail") # , , , , , "Belly"  "Fillet" "Loin"

## Quality control and summary statistics ##########################################

Workfile[1:10,]
dim(Workfile)
typeof(Workfile)

n <- dim(Workfile)[1] # number of fillets (two fillets per fish)

# I am looking for some specific features to check whether there is a difference in conventional vs sequential codend.
colnames(Workfile)

# The 'response', Y, (also known as the dependent variable) is the amount of blood, which corresponds to the feature 'Fillet'/'Loin'/'Belly'/'Tail. 
# As 'predictors', $x_1, x_2, \ldots$, (also known as independent variables) we have: 'Pair', 'Codend', 'Bleeding time (minutes)'. 
# 'Pair' refers to haul pair, and can be split into 'Catching depth (m)', 'Towing time (min)', and 'Catch size', which is the information we have for each haul.
# First, I'm gonna rename them (and check'em)

# Small function finding the response variable Y
resp_var = function(variable){
  match(variable,names(Workfile))
}

response = resp_var(resp_var_name)
response

# Original names of predictors, and the idx vector
orig_pred_strng <- c("Pair","Codend","Bleeding time (minutes)", "Catching depth (m)", "Towing time (min)", "Catch size")
pred_vec <- match(orig_pred_strng,names(Workfile))

# New names
pred_strng <- c("Pair","Codend","Time","Depth", "Towing", "Size")
for (i in 1:length(orig_pred_strng)){
  colnames(Workfile)[match(orig_pred_strng[i],names(Workfile))] <- pred_strng[i]
}

Workfile[,c(response,pred_vec)]

# Checking for missing values

missing = which(is.na(unlist(Workfile[,c(response,pred_vec)])))
length(missing)

# There are two fillets per fish. I'm taking the mean value for each fish, so that we don't have that dependency. 

NewWorkfile = data.frame(matrix(NA, nrow = n/2, ncol = length(pred_vec)+length(resp_var_name)))


# NewWorkfile = Workfile[1:(n/2),c(response,pred_vec)] # Bad way to declare a variable
colnames(NewWorkfile) = c(resp_var_name, pred_strng)

resp = match(resp_var_name, colnames(NewWorkfile))
pred = match(pred_strng, colnames(Workfile))

ii = 0
for (i in 1: n){   #n
  if (i%%2 == 0) {
    ii = ii+1
    #    print(c(ii,i))
    NewWorkfile[ii,resp] = (Workfile[i-1,response]+Workfile[i,response])/2
    for (j in 1: length(pred_strng)){
      NewWorkfile[ii, j+1] = Workfile[i,pred[j]]
    }
  }
}

NewWorkfile = as_tibble(NewWorkfile)
NewWorkfile

# Then, I'm checking them one by one, checking that they are categorical or continuous. 
# I'm displaying the first values for each feature, and a frequency table or histogram. 

##### Fillet
# We expect the historgrams of mean fillet and fillet to look similar, but not exactly the same.
print(NewWorkfile[1:5,resp])
print(Workfile[1:5,response])
hist(NewWorkfile[[resp]])
hist(Workfile[[response]])

##### Pair
print(table(NewWorkfile$Pair))
print(table(Workfile$Pair))
print(NewWorkfile$Pair[1:5])
# One fish is missin from haul nr 9. 

##### Codend
# From character to numeric.
print(table(NewWorkfile$Codend))
print(table(Workfile$Codend))
NewWorkfile$Codend[which(NewWorkfile$Codend=="Conventional")] <- 0
NewWorkfile$Codend[which(NewWorkfile$Codend=="Sequential")] <- 1
NewWorkfile$Codend = as.numeric(NewWorkfile$Codend)
print(NewWorkfile$Codend[1:15])

##### Time
print(table(NewWorkfile$Time))
print(table(Workfile$Time))
print(NewWorkfile$Time[1:5])

##### Depth
print(table(NewWorkfile$Depth))
print(table(Workfile$Depth))
print(NewWorkfile$Depth[1:5])

##### Towing
print(table(NewWorkfile$Towing))
print(table(Workfile$Towing))
print(NewWorkfile$Towing[1:5])

##### Size
print(table(NewWorkfile$Size))
print(table(Workfile$Size))
print(NewWorkfile$Size[1:15])

## Spearman correlation
# I'm having a look at the ordinal correlations. Just the numeric variables. 
# And comparing them to the pearson correlation. 
# This is to see if it is justified to use continuous (instead of categorical) variables
# in the regression analysis later. 

# Function for correlation and p-value
corr_fun = function(datatable, pear){
  
  spearman_cor = cor(datatable, method = "spearman")
  print(spearman_cor)
  
  if (pear==1){
    pearson_cor = cor(datatable, method = "pearson")
    print(pearson_cor)
  }
  
  p_values = matrix(0,dim(datatable)[2], dim(datatable)[2])
  colnames(p_values) = colnames(datatable)
  rownames(p_values) = colnames(datatable)
  suppressWarnings(
    for (i in 1: dim(datatable)[2]){
      for (j in i: dim(datatable)[2]){
        if (j != i) {
          test = cor.test(datatable[[i]], datatable[[j]], method="spearman")
          p_values[i,j] = test$p.value
        }
      }
    }
  )
  print(p_values)
}

corr_fun(NewWorkfile, 0)

# Keep in mind that the above analysis is one-by-one analysis. 

# We see that Time has the strongest correlation with Fillet, both in absolute values and significance. 
# The codend is positively correlated, which means that going from Conventional to Sequential increases the 
# amount of blood (opposite of what we expected, no?). And the p-value is 0.0539, which is quite small. 

# The difference between Spearman and Pearson is quite small, except for Codend-Catch. 
# The Codend-Catch correlation is huge, and sadly, this will ruin all conclusions regarding the Codend's influence 
# on blood amount in fillets. 

#### Codend-Catch
plot(NewWorkfile$Codend, NewWorkfile$Size)

##### Codend-Depth
plot(NewWorkfile$Codend, NewWorkfile$Depth)

##### Codend-Towing
plot(NewWorkfile$Codend, NewWorkfile$Towing)

# So, all kosepose catches are larger than the corresponding conventional trawl codend. 
# Actually, all kosepose catches are larger than all conventional catches.

## Generalized linear regression
# I'm gonna to a linear regression anyhow. There is an infinite number of models, so the choice of GLM needs some 
# justification. In short we need a model that i) simple: so that each variable (Time, Codend, etc) can be analysed
# ii) nested: meaning that we can add/remove one variable at the time and analyse the effect. We do not need the 
# "best" model, because we are not aiming at building a classifier. GLMs are simple, they allow for nesting, and 
# most reviewers are familiar with them. 

##### Haul pair model
# First, we do a linear regression with 'Pair'.

NewWorkfile$Codend = factor(NewWorkfile$Codend)
NewWorkfile$Pair = factor(NewWorkfile$Pair)

glm_fun = function(form, datatable, coeff=0, anov=0){
  print(form)
  glm_full <- glm(formula = form, data=datatable,
                  family=gaussian(link = "identity"))
  # Testing if the residuals are very different from the Normal distribution
  res_full = resid(glm_full, type="response")
  res_std = (res_full-mean(res_full))/sqrt(var(res_full))
  hist(res_std,breaks = 20)
  norm = ks.test(res_std,pnorm) # Kolmogorov-Smirnov test
  print(c('norm: ',norm$p.value))
  sum_full = summary(glm_full)
  if (coeff){
    print(sum_full$coefficients)
    print(confint(glm_full))
  }
  if (anov){
    anov_full = anova(glm_full, test = "LRT")
    anov_full$`Pr(>Chi)`
  }
}

x <- c("Time+Codend+Pair")
form <- reformulate(x, response = resp_var_name)
glm_fun(form, NewWorkfile, 1,1)

# The 'Codend' slope  is different from zero.
# And from the anova analysis, it looks like 'Pair' does not add much. 

##### Full model
# But what happens if we use 'Depth', 'Towing' and 'Size' instead of pair?

x <- c("Time+Depth+Size+Towing+Codend")
form <- reformulate(x, response = resp_var_name)
glm_fun(form, NewWorkfile, 1,1)

# 'Codend' looses its significance. This means that the catch size is a better explanation for the variation in blood amount 
# than codend type. But there is a problem: The catch size is unique for each haul, meaning that it can predict perfectly Codend, 
# Towing and Depth, otherwise known as singularity. It doesn't show when we let Catch be continuous. 

# Anova: Towing, Size and Depth
x <- c("Time+Depth+Size+Codend+Towing")
form <- reformulate(x, response = resp_var_name)
glm_fun(form, NewWorkfile, anov=1)

x <- c("Time+Size+Codend+Towing+Depth")
form <- reformulate(x, response = resp_var_name)
glm_fun(form, NewWorkfile, anov=1)

x <- c("Time+Codend+Towing+Depth+Size")
form <- reformulate(x, response = resp_var_name)
glm_fun(form, NewWorkfile, anov=1)

# Having a look at the standardized coefficients:
library(reghelper)
glm_full <- glm(formula = form, data=NewWorkfile,
                family=gaussian(link = "identity"))
beta(glm_full)$coefficients

### Codend by codend
#Let's have a look at Catch for the separate codends.

##### Conventional Catch

ConvWorkfile = NewWorkfile[NewWorkfile$Codend==0,]
ConvWorkfile = select(ConvWorkfile, -c(Pair, Codend))

corr_fun(ConvWorkfile, 1)
x <- c("Time") # +Towing+Depth+Size
form <- reformulate(x, response = resp_var_name)
glm_fun(form, ConvWorkfile, coeff=1)

# What we see here is that within conventional codend, catch size is not helping.

##### Sequentional Catch
SeqWorkfile = NewWorkfile[NewWorkfile$Codend==1,]
SeqWorkfile = select(SeqWorkfile, -c(Pair, Codend))

corr_fun(SeqWorkfile, 1)
# x <- c("Time+Towing+Depth+Size")
form <- reformulate(x, response = resp_var_name)
glm_fun(form, SeqWorkfile, coeff=1)


### Bleeding Time
#Let's have a look at Time for the separate codends.
# Data does not support that there is a difference in the Bleeding effect for conv vs seq codend if 
# treated as continuous variable.

ci_t_fun = function(datavector, alpha){
  meen = mean(datavector)
  s = var(datavector)
  n = length(datavector)
  df = n-1
  t_a = qt(1-alpha/2, df)
  low = meen - t_a*s/sqrt(n)
  up = meen + t_a*s/sqrt(n)
  print(c(low,up))
}

t = c(0,20,40,60)

for (i in 1: length(t)){
  Time = NewWorkfile[NewWorkfile$Time==t[i],]  
  ci = ci_t_fun(Time[[resp]],0.05)
}


for (i in 1: length(t)){
  Time = ConvWorkfile[ConvWorkfile$Time==t[i],]  
  ci = ci_t_fun(Time[[resp]],0.05)
}

for (i in 1: length(t)){
  Time = SeqWorkfile[SeqWorkfile$Time==t[i],]  
  ci = ci_t_fun(Time[[resp]],0.05)
}

plot(ConvWorkfile$Time,ConvWorkfile[[resp]],col="blue",
     xlab="Time from catch to bleeding (min)", 
     ylab="Hemoglobin (mg Hb/g muscle)")
points(SeqWorkfile$Time,SeqWorkfile[[resp]],col="red")
form <- reformulate("Time", response = resp_var_name)
glm_full <- glm(formula = form, data=NewWorkfile,
                family=gaussian(link = "identity"))
abline(glm_full$coefficients[1],glm_full$coefficients[2])
glm_full <- glm(formula = form, data=ConvWorkfile,
                family=gaussian(link = "identity"))
abline(glm_full$coefficients[1],glm_full$coefficients[2], col="blue")
glm_full <- glm(formula = form, data=SeqWorkfile,
                family=gaussian(link = "identity"))
abline(glm_full$coefficients[1],glm_full$coefficients[2], col="red")
legend("topleft", 
       legend = c("conv", "seq", "both"), 
       col = c(rgb(0,0,1), 
               rgb(1,0,0),
               rgb(0,0,0)), 
       lty = 1, 
       bty = "n", 
       pt.cex = 2, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0, 0))

print("fin de semain")

