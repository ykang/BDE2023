##加载程序包
library(mice)                                     
#mice是实现多重插补的程序包，我们将调用其中的mice和md.pattern函数。
library(VIM)                                      
#VIM是展示数据缺失模式可视化的程序包，我们将调用其中的aggr函数。
library(dplyr)
#dplyr是数据处理的程序包，我们将调用其中的管道函数。

##读入数据，生成R数据框
Air <- read.csv("ch3_air.csv",colClasses=rep("numeric",4))
#rep函数将字符串"numeric"重复4次得到一个长度为4的向量。
#colClasses=rep("numeric",4)指定4个变量的类型均为数值型。

##查看Air数据集各变量的基本情况
summary(Air)
#观察到Air的前两个变量Ozone和SolarR分别有37和7个缺失值

##查看数据缺失模式
md.pattern(Air)                                   
#md.pattern函数以矩阵形式展示缺失模式，输出结果如下：
#      Wind Temp  SolarR Ozone   
#  111    1    1       1     1   0
#  35     1    1       1     0   1
#  5      1    1       0     1   1
#  2      1    1       0     0   2
#         0    0       7    37  44
#其解释如下：
#  缺失模式中，1表示对应变量被观测到，0表示对应变量缺失。
#  最左边列表示对应缺失模式的观测数，
#  最右边列表示对应缺失模式的缺失变量个数。
#  例如，有35个观测的Wind、Temp、SolarR这三个变量都被观测到，
#    而Ozone变量缺失，每个这样的观测有一个变量缺失。
#  最后一行表示对应每列的有缺失值的观测数。

##将Air数据集的缺失情况可视化
aggr(Air,prop=T,numbers=T,col=c('white','grey'))                        
#aggr函数将数据缺失情况可视化呈现，
#  左图给出每个变量的缺失情况，右图给出每种缺失模式的情况。
#  prop=T指定左图纵坐标为缺失比例，
#  numbers=T指定右图显示对应缺失模式所占的比例。
#  col=c('white','grey')指定观测值和缺失值分别用白色和灰色表示。

##对Air数据集进行多重插补。
m <- 5
#指定插补重数为5。

mi_Air <- mice(Air,m)              
#mice函数实现多重插补，插补后的数据存入mi_Air。

##对插补后的数据集work.mi_Air进行综合分析，
##估计Ozone和SolarR这两个变量的总体均值，并得到估计的标准误差。
with(mi_Air,lm(Ozone~1)) %>% pool() %>% summary()
#lm函数用于建立线性回归模型，
#  lm(Ozone~1)表示以Ozone为因变量，常数项1为自变量建立线性回归模型，
#  截距项的估计实际上就是对Ozone变量的总体均值的估计。
#with函数对mi_Air的每个插补数据集都进行回归分析。
#使用管道函数“%”和pool函数对mi_Air中各个插补数据集的结果进行综合分析。
#使用管道函数“%”和summary函数输出综合分析的结果，输出结果中，
#  est为截距项（Intercept）的综合估计值，
#     也就是对Ozone变量的总体均值的综合估计；
#  se为对Ozone变量的总体均值的综合估计的标准误差。

with(mi_Air,lm(SolarR~1)) %>% pool() %>% summary()
