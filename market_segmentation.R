url <- "https://bit.ly/3fOGZPu"
df <- read.table(url, header = T, sep = ",", encoding = "UTF-8")
suppressMessages(install.packages("tidyr"))
library(tidyr)
df2 <- spread(df, key = brand, value = sales)

df3 <- df2

df3[is.na(df3)] <- 0


##Transposing the data frame to perform Q-type Factor Analysis
df3.T <- t(df3[,2:ncol(df3)])
colnames(df3.T) <- t(df3[,1])

df3.T <- as.data.frame(df3.T)

cor_mat <- cor(df3.T)

#Bartlett's test of sphericity
suppressMessages(install.packages("psych"))
library(psych)
cortest.bartlett(cor_mat,n=nrow(df3.T))

#KMO measure of sample adequacy, less 0.05 is unacceptable
KMO(cor_mat)

#Excluding those with KMO less than 0,5
#df3.T2 <- select(df3.T, -Peru)
df3.T2 = subset(df3.T, select = -c(Peru) )

cor_mat2 <- cor(df3.T2)
cortest.bartlett(cor_mat2, n=nrow(df3.T2))
KMO(cor_mat2)

#Extracting the factors
efa <- fa(df3.T2, fm='pa',rotate='none')
print(efa)

#Determining the number of factors to be retained
plot(efa$values, type = 'b', xlab = 'Factor', ylab = 'Eigenvalue'
     , col='blue')
abline(h=1, lty=2, col='red')

#Re-extracting the factors
efa2 <- fa(df3.T2, fm='pa',nfactors = 2, rotate = 'none')
print(efa2)

##Matrix of significant loadings
print(efa2$loadings, cutoff = 0.3)

#Rotating the factors
efa2 <- fa(df3.T2, fm='pa',nfactors = 2, rotate = 'varimax')
print(efa2$loadings, cutoff = 0.3)

#Improve the model adjustment
#Getting the communalities
print(efa2$communality)

#Removing the communalities less than 0.5
#df3.T3 <- select(df3.T2, -Chile)

df3.T3 = subset(df3.T2, select = -c(Chile) )
cor_mat3 <- cor(df3.T3)
cortest.bartlett(cor_mat3, n=nrow(df3.T3))
KMO(cor_mat3)

#Extracting the factors
efa3 <- fa(df3.T3, fm='pa',rotate='none')
print(efa)

#Determining the number of factors to be retained
plot(efa3$values, type = 'b', xlab = 'Factor', ylab = 'Eigenvalue'
     , col='blue')
abline(h=1, lty=2, col='red')
efa3 <- fa(df3.T3, fm='pa',nfactors = 2, rotate = 'varimax')
efa3$communality[order(efa3$communality)]


#Delete other countries
df4.T3 <- select(df3.T3, -'Japan',-'Colombia',-'Brazil',-'USA',-'Argentina',
                 -'China',-'Mexico',-'Russia',-'United Arab Emirates')
cor_mat4 <- cor(df4.T3)
cortest.bartlett(cor_mat4, n=nrow(df4.T3))
KMO(cor_mat4)

#Extracting the factors
efa4 <- fa(df4.T3, fm='pa',rotate='none')
print(efa)

#Determining the number of factors to be retained
plot(efa4$values, type = 'b', xlab = 'Factor', ylab = 'Eigenvalue'
     , col='blue')
abline(h=1, lty=2, col='red')
efa4 <- fa(df4.T3, fm='pa',nfactors = 2, rotate = 'varimax')
efa4$communality[order(efa4$communality)]

#Plotting the factor loadings
loadings <- as.data.frame(unclass(efa4$loadings))
loadings$country <- rownames(loadings)
rownames(loadings) <- NULL

loadings$country <- factor(loadings$country)
loadings$country <-reorder(loadings$country, loadings$PA1)

loadings2 <- gather(loadings, key=factor, value = loading,-country)

suppressMessages(install.packages("ggplot2"))
library(ggplot2)
ggplot(loadings2)

corrs<-as.data.frame(unclass(cor_mat4))
corrs$country1 <-rownames(corrs)
corrs$country1 <- factor(corrs$country1)
corrs$country <-reorder(corrs$country, loadings$PA1)
corrs_long <- gather(corrs, key=country2, value = corr,-country1)
