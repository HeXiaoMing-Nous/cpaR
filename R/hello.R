# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}
####速算扣除数的函数####
####速算扣除数的函数####
#a <- c(0, 20000, 50000)#预扣预缴应纳税所得额各档次的下限（不含）
#b <- c(0.2, 0.3, 0.4)#各档次预扣率
#d <- c()#速算扣除数
#所得额级差 <- c()#不同级数所得额之间的差额
#for (i in 1:(length(a)-1)) {
#  所得额级差[i] <- a[i+1] - a[i]
#}
#速算扣除数 <- c()
#for (i in 1:(length(a)-1)) {
#  速算扣除数[i] <- sum((b[i+1]-b[1:i])*所得额级差[1:i])
#}
#速算扣除数 <- append(速算扣除数,0,0)
#Quick calculation of deductions
#ykynssde：预扣预缴应纳税所得额a
#ykl：预扣率b
#ljykyjynssde：累计预扣预缴应纳税所得额
quicalde <- function(a, b) {#计算速算扣除数函数Quick calculation of deductions
  所得额级差 <- c()#不同级数所得额之间的差额
  for (i in 1:(length(a)-1)) {
    所得额级差[i] <- a[i+1] - a[i]
  }
  速算扣除数 <- c()
  for (i in 1:(length(a)-1)) {
    速算扣除数[i] <- sum((b[i+1]-b[1:i])*所得额级差[1:i])
  }
  速算扣除数 <- append(速算扣除数,0,0)
  return(速算扣除数)
}

cumintax <- function(a, b, d) {#计算累计个人所得税函数Cumulative personal income tax
  if(missing(a)) a <- c(0,36000,144000,300000,420000,660000,960000)
  if(missing(b)) b <- c(3,10,20,25,30,35,45)*0.01
  所得额级差 <- c()#不同级数所得额之间的差额
  for (i in 1:(length(a)-1)) {
    所得额级差[i] <- a[i+1] - a[i]
  }
  速算扣除数 <- c()
  for (i in 1:(length(a)-1)) {
    速算扣除数[i] <- sum((b[i+1]-b[1:i])*所得额级差[1:i])
  }
  速算扣除数 <- append(速算扣除数,0,0)
  maxid <- c()
  #累计收入 <- c()
  for (i in 1:length(d)) {
    #累计收入 <- cumsum(d)
    maxid[i] <- max(which(a<d[i]))#等级索引号
  }
  预扣税额 <- c()
  #本期应预扣税额 <- c()
  for (i in 1:length(d)){
    预扣税额[i] <- d[i]*b[maxid[i]]-速算扣除数[maxid[i]]
    #本期应预扣税额[i] <- 预扣税额[i]-sum(预扣税额[1:i-1])
  }
  return(预扣税额)
}

intax <- function(a, b, d) {#计算个人所得税函数income tax for individuals
  if(missing(a)) a <- c(0,36000,144000,300000,420000,660000,960000)
  if(missing(b)) b <- c(3,10,20,25,30,35,45)*0.01
  所得额级差 <- c()#不同级数所得额之间的差额
  for (i in 1:(length(a)-1)) {
    所得额级差[i] <- a[i+1] - a[i]
  }
  速算扣除数 <- c()
  for (i in 1:(length(a)-1)) {
    速算扣除数[i] <- sum((b[i+1]-b[1:i])*所得额级差[1:i])
  }
  速算扣除数 <- append(速算扣除数,0,0)
  maxid <- c()
  #累计收入 <- c()
  for (i in 1:length(d)) {
    #累计收入 <- cumsum(d)
    maxid[i] <- max(which(a<d[i]))#等级索引号
  }
  预扣税额 <- c()
  本期应预扣税额 <- c()
  for (i in 1:length(d)){
    预扣税额[i] <- d[i]*b[maxid[i]]-速算扣除数[maxid[i]]
    本期应预扣税额[i] <- 预扣税额[i]-sum(预扣税额[1:i-1])
  }
  本期收入 <- d[1]
  if(length(d)!=1){
    for (i in 2:length(d)){
      本期收入[i] <- d[i]-d[i-1]
    }
  }
  个人每月预缴所得税明细 <- data.frame(累计收入=d,
                                本期收入=本期收入,
                                预扣税额=预扣税额,
                                本期应预扣税额=本期应预扣税额,
                                stringsAsFactors = FALSE)
  #本期预扣预缴税额 <- ifelse(length(应纳税额)=1,应纳税额[i],应纳税额[i]-应纳税额[i-1])
  return(个人每月预缴所得税明细)
}

#全员预缴所得税明细
#qy <- function(e) {
#  bqykyjse(d=ljsr$累计收入)

#}

#e <- c(0,3000,12000,25000,35000,55000,80000)
#f <- c(3,10,20,25,30,35,45)*0.01

#ljsr <- data.frame(姓名=c("张三","李四"),
#             累计收入=c(2600,2600))
