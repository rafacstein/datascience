# install and load package
install.packages("Synth") 
library(Synth)
# read the dataset "basque"
data("basque")
#EDA
dim(basque) #774*17
basque[1:10,]
#The original dataset "basque" has a traditional panel format, and we need to read it in another form for using synth().
# set up different arguments
# foo:  
dataprep.out <- dataprep(foo = basque,
                         predictors = c("school.illit", "school.prim", "school.med",
                                         "school.high", "school.post.high", "invest"),
                         predictors.op = "mean", # the operator
                         time.predictors.prior = 1964:1969, #the entire time frame from the #beginning to the end
                         special.predictors = list(
                           list("gdpcap", 1960:1969, "mean"),
                           list("sec.agriculture", seq(1961,1969,2),"mean"),
                           list("sec.energy",seq(1961,1969,2),"mean"),
                           list("sec.industry", seq(1961,1969,2),"mean"),
                           list("sec.construction", seq(1961,1969,2),"mean"),
                           list("sec.services.venta", seq(1961,1969,2),"mean"),
                           list("sec.services.nonventa",seq(1961,1969,2),"mean"),
                           list("popdens", 1969, "mean")),
                         dependent = "gdpcap", # dv
                         unit.variable = "regionno",#identifying unit numbers
                         unit.names.variable = "regionname",#identifying unit names
                         time.variable = "year",#time-periods
                         treatment.identifier = 17,#the treated case
                         controls.identifier = c(2:16, 18),#the control cases; all others #except number 17
                         time.optimize.ssr = 1960:1969,#the time-period over which to optimize
                         time.plot = 1955:1997)#the entire time period before/after the treatment

synth.out = synth(data.prep.obj = dataprep.out, method = "BFGS")


gaps = dataprep.out$Y1plot - (dataprep.out$Y0plot 
                              %*% synth.out$solution.w)
gaps[1:3,1]

synth.tables = synth.tab(dataprep.res = dataprep.out,
                         synth.res = synth.out)
names(synth.tables)
[1] "tab.pred" "tab.v"    "tab.w"    "tab.loss"

synth.tables$tab.pred[1:13,]

synth.tables$tab.w[8:14, ]

# plot the changes before and after the treatment 
path.plot(synth.res=synth.out,dataprep.res = dataprep.out, 
          Ylab="real per-capita gdp (1986 USD, thousand)",Xlab="year",
          Ylim = c(0,12),Legend = c("Basque country", 
                                    "synthetic Basque country"),
          Legend.position = "bottomright")

gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "gap in real per-capita GDP (1986 USD, thousand)", Xlab= "year",
          Ylim = c(-1.5,1.5), Main = NA)