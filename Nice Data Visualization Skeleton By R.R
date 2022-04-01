

########Summary Statistics based on Dry beans class###################
cl.summary<- bean.dat %>%
  group_by(Class) %>%
  summarise(across(where(is.numeric), mean))
kable(cl.summary)

###########################Bar Plot######################
ggplot(bean.dat) + aes(x = Class, y = Area, fill = Class) + 
  stat_summary(geom = "bar", fun = "mean") + stat_summary(geom = "errorbar", 
                                                          fun.data = "mean_se", 
                                                          width = .3)


############################ Correlation matrix########################
library(corrplot)
library(RColorBrewer)
M <-cor(num.bean)
# as number
corrplot(M, method="number")



##############Mutiple Histograms########################
#Histogram using base R for numeric variables
num.bean <- bean.dat [,-8] #selecting numeric variables only
####################Plotting Histogram for all Numeric Variables############
plotHist <- function(columns,bin,colours){
  par(mfrow = c(3,3))#Histogram plots to visualize the distribution of the numeric variables in the data set.
  for (i in columns) {
    hist(num.bean[,i], main = paste("Histogram of ", names(num.bean)[i]),
         nclass = bin, las = 1, col = colours, 
         xlab = paste(names(num.bean)[i]))
  }
}

plotHist(c(1:7), bin = 60, "brown")

####################Multiple diagnostic Plot######################
variable_1 <- c("Pregnancies", "Glucose", "Insulin", "Age") 
par(mfrow = c(2, 2)) 
for(i in variable_1) { 
  qqnorm(diab.yes[[i]]); qqline(diab.yes[[i]], col = 2) 
}


###################Multiple Boxplot########################

plot <- list()
box_variables <- c("Pregnancies", "Age", "Glucose")
for(i in box_variables) {
  plot[[i]] <- ggplot(diabetes, 
                      aes_string(x = "Outcome", 
                                 y = i, 
                                 col = "Outcome", 
                                 fill = "Outcome")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c("blue", "red")) +
    scale_fill_manual(values = c("blue", "red"))
}
do.call(grid.arrange, c(plot, nrow = 1))

#########################Multiple Density Plot#####################

plot <- list() 
for(i in names(diabetes)[-9]) { 
  plot[[i]] <- ggplot(diabetes, aes_string(x = i, y = "..density..", col = "Outcome")) + 
    geom_density(aes(y = ..density..)) + 
    scale_color_manual(values = c("blue", "red")) + 
    theme(legend.position = "none") 
} 
do.call(grid.arrange, c(plot, nrow = 4))


#Density plot using ggplot tool for numeric variables
dp.B<- ggplot(bean.dat,aes(x=Perimeter,fill=Class))+geom_density(col=NA,alpha=0.40)
dp.C<- ggplot(bean.dat, aes(x= MajorAxisLength, fill= Class))+ geom_density(col= NA, alpha= 0.40)
dp.D<- ggplot(bean.dat, aes(x=MinorAxisLength, fill= Class))+ geom_density(col= NA, alpha= 0.40)
dp.E<- ggplot(bean.dat, aes(x= Eccentricity, fill= Class))+ geom_density(col= NA, alpha= 0.40)
dp.F<- ggplot(bean.dat, aes(x= ConvexArea, fill= Class))+ geom_density(col= NA, alpha= 0.40)
dp.G<- ggplot(bean.dat, aes(x= Extent, fill= Class))+ geom_density(col= NA, alpha= 0.40)
gridExtra::grid.arrange(dp.B,dp.C,
                        dp.D,dp.E,dp.F,
                        dp.G, ncol=2)