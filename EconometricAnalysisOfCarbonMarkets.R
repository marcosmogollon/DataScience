1 path<-"C:\Users\Alexander\Desktop 2\Lectures\Books+Presentations\Econometric Analysis of Carbon Markets\Data"
2 setwd(path)
3 library(dynlm)
4 data=read.csv("data_chapter2.csv",sep=",")
5 attach(data)
6 eua=data[,2]
7 napII=data[,3]
8 napIII=data[,4]
9 model<-dynlm(eua~L(eua)+napII+napIII)
10 summary(model)
11 layout(matrix(1:4,2,2))
12 plot(model)
13 acf(residuals(model))
14 acf(residuals(model),type='partial')