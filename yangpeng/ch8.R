##加载程序包
library(dplyr)
#我们将调用其中的管道函数和mutate函数。
library(glmnet)
#glmnet是建立Lasso模型和Elastic Net模型的程序包,
#我们将调用其中的glmnet和cv.glmnet函数。


##读入数据，存为R数据框stock。
stock <- read.csv("ch8_stock.csv")


##数据预处理。
stock <- stock %>% mutate(ISE1=lag(ISE,1)) %>% mutate (ISE2=lag(ISE,2)) %>%
  mutate(SP1=lag(SP,1)) %>% mutate(SP2=lag(SP,2)) %>%
  mutate(DAX1=lag(DAX,1)) %>% mutate(DAX2=lag(DAX,2)) %>%
  mutate(FTSE1=lag(FTSE,1)) %>% mutate(FTSE2=lag(FTSE,2)) %>%
  mutate(NIKKEI1=lag(NIKKEI,1)) %>% mutate(NIKKEI2=lag(NIKKEI,2)) %>%
  mutate(BOVESPA1=lag(BOVESPA,1)) %>% mutate(BOVESPA2=lag(BOVESPA,2)) %>%
  mutate(EU1=lag(EU,1)) %>% mutate(EU2=lag(EU,2)) %>%
  mutate(EM1=lag(EM,1)) %>% mutate(EM2=lag(EM,2))
#创建8个指数滞后一至两天的变量。
#  使用mutate函数加入新的变量。
#  lag(ISE,1)获取将ISE滞后一天的变量，lag(ISE,2)获取将ISE滞后两天的变量，等等。

stock <- stock[ , !(names(stock) %in% 
                      c("SP","DAX","FTSE","NIKKEI","BOVESPA","EU","EM"))]
#将7个国际指数的原始未滞后的变量删除，因为之后的分析不会用到这些变量。
#  names(stock)给出stock各变量的名称。
#  names(stock) %in% c("SP","DAX","FTSE","NIKKEI","BOVESPA","EU","EM")
#    得到一个向量，当变量名称属于7个国际指数时，相应元素取值为TRUE，
#    否则取值为FALSE。
#  使用!进行反操作，当变量名称不属于7个国际指数时，相应元素取值为TRUE，
#    否则取值为FALSE。
#  最后获取对应值为TRUE的stock中的所有变量。


##建立Lasso模型
x <- model.matrix(ISE~.,stock[,-1])[,-1]
#glmnet函数要求自变量的格式为矩阵，自变量都是数值型。
#这里用model.matrix函数得到自变量的矩阵，该函数会自动将分类变量转换为哑变量。
#  （如果一个分类变量有k个类别，会生成k-1个哑变量）。
#  stock[,-1]指定使用的数据为stock中去除第一个变量（日期）的数据。
#  ISE~.指定数据中ISE为因变量，其他变量均为自变量。
#model.matrix函数所得的第一列对所有观测取值都是1，所以去除该列。

y <- stock$ISE[-(1:2)]
#glmnet函数要求因变量的格式为数值向量。
#因变量为ISE。因为第一、二个观测滞后两天的自变量值不存在，所以去除这两个观测。

lassofit <- glmnet(x,y)
#使用glmnet函数建立lasso模型


##查看建模结果
print(lassofit)
#屏幕上将显示Lasso每一步的结果摘要。
#  Lamdba表示调节参数λ的值；
#  DF表示给定调节参数λ的值时，所得模型中非零系数的个数；
#  %Dev表示偏差中被所得模型解释的比例。

plot(lassofit,xvar = "lambda",label = T,col="black")
#可视化展示Lasso每一步的结果。
#  xvar="lambda"指定横轴为调节参数λ的对数。
#  图中每条曲线代表一个自变量，展示该变量的估计系数如何随着λ的对数值的变化
#    而变化。
#  上面的横轴表示非零系数的个数。
#  label=T指定在图中标明每条曲线对应的自变量序号。
#  col="black"指定曲线颜色为黑色。


##使用交叉验证法选择最佳模型
cvfit <- cv.glmnet(x,y)
#cv.glmnet函数使用交叉验证选出调节参数λ的最佳值，
#  得到相应的最佳模型。

plot(cvfit)
#该图展示了交叉验证的平均误差如何随λ的对数值的变化而变化。
#  其中两条虚线对应于λ的如下两个值：
#  （1）使交叉验证的平均误差最小的λ值；
#  （2）使交叉验证的平均误差在其最小值1个标准误差范围内的最大的λ值。

cvfit$lambda.min
#使交叉验证的平均误差最小的λ值。
cvfit$lambda.1se
#使交叉验证的平均误差在其最小值1个标准误差范围内的最大的λ值。

y.pred <- predict(cvfit,x,s="lambda.min")
#用交叉验证结果给出的λ值对应的模型进行预测。
#  s="lambda.min"表示使用使交叉验证的平均误差最小的λ值。
#  这里自变量的矩阵为对训练观测计算的矩阵，也可以使用其他观测对应的矩阵。

coef(cvfit,s="lambda.min")
#交叉验证结果给出的λ值对应的模型的回归系数
#  s="lambda.min"表示使用使交叉验证的平均误差最小的λ值。



##建立Elastic Net模型。
foldid <- sample(1:10,size = length(y),replace = T)
#将数据随机划分为10份。
#  从1到10这10个数中可重复地（replace=T）进行随机抽取，
#  得到一个长度为观测个数的向量，
#  每个元素给出了相应观测所属的10份数据集中的数据集序号。

error <- rep(0,11)
for(j in 1:11)
{
  fit <- cv.glmnet(x,y,foldid = foldid,alpha=(j-1)/10)
  error[j] <- min(fit$cvm)
}
#cv.glmnet函数中，用alpha表示ρ的值，需要指定这个值（缺省为1，对应于Lasso）。
#  我们需要自己编写程序寻找ρ的值。
#    这里考虑ρ=0,0.1,0.2,...,1。
#    应保证不同的ρ对应的是同一份交叉验证数据，因此需提前
#    对数据划分好，并传递给foldid参数。
#error向量记录ρ的每个值对应的最小的交叉验证误差。

best.rho <- (which.min(error)-1)/10 
#寻找使交叉验证的平均误差最小的ρ的值。对这个例子而言，
#  恰巧最佳的ρ值为1，对应于Lasso。



## 将因变量转换为二值变量，取值1表示大于0，取值-1表示小于0
y2 <- sign(y)

##使用交叉验证找最佳模型
cvfit2 <- cv.glmnet(x,y2,family = "binomial",type.measure = "class")
#family = "binomial"指定因变量分布为二项分布。
#type.measure="class"指定交叉验证的准则为错误分类率。

plot(cvfit2)

table(y2,predict(cvfit2,x,s = "lambda.min",type = "class"))
#查看当λ值使交叉验证的平均误差最小时，模型的混淆矩阵。
#  y2为因变量的真实值；
#  使用predict函数获取因变量的预测值。

coef(cvfit2,s="lambda.min")


