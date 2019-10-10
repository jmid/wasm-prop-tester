setwd("~/SDU/Master thesis/Project/plot")

# 1557041683 sized backtrack
# 1557043141 500   backtrack
# 1557046598 1000  backtrack

data <- read.table('1557041683.backtrack')
data2 <- read.table('1557043141.backtrack')
data3 <- read.table('1557046598.backtrack')
data[[1]]
typeof(data[1])
typeof(as.vector(data[1]))

data.numbers <- as.numeric(as.vector(as.character(data)))

mean(data3[[1]])

median(data3[[1]])

plot(data.numbers, type = "h")

df <- data.frame(value = as.list(data.numbers))
df

typeof(data)

length(data)
length(as.list(data))

data.aggregated = aggregate(data2, data, sum)

max(data3[[1]])
min(data[[1]])

plot(table(data[[1]]))


typeof(data[[1]])
length(data[[1]])

a <- c(data[[1]])
typeof(a)


hist(data[[1]])
barplot(table(data[[1]]))
barplot(table(data2[[1]]))
barplot(table(data3[[1]]))

data.frame(table(data2[[1]]))$Freq
table(data[[1]])
table(data2[[1]])
table(data3[[1]])

max(as.numeric(as.vector(data.frame(table(data[[1]]))$Var1)))

as.numeric(as.vector(as.character(data.frame(table(data[[1]]))[1])))

l <- c(0,2)
l[1] <- 2
l

maxno <- max(
  max(as.numeric(as.vector(data.frame(table(data[[1]]))$Var1))),
  max(as.numeric(as.vector(data.frame(table(data2[[1]]))$Var1))),
  max(as.numeric(as.vector(data.frame(table(data3[[1]]))$Var1))));

data.frame(sized=table(data[[1]]), size500=table(data2[[1]]), size1000=table(data3[[1]]))

sized <- data.frame(
  var1=as.numeric(as.vector(data.frame(table(data[[1]]))$Var1)),
  freq=as.numeric(as.vector(data.frame(table(data[[1]]))$Freq)));

size500 <- data.frame(
  var1=as.numeric(as.vector(data.frame(table(data2[[1]]))$Var1)),
  freq=as.numeric(as.vector(data.frame(table(data2[[1]]))$Freq)));

size1000 <- data.frame(
  var1=as.numeric(as.vector(data.frame(table(data3[[1]]))$Var1)),
  freq=as.numeric(as.vector(data.frame(table(data3[[1]]))$Freq)));

sized[1,]
sized[1,2]

if (3 %in% sized$var1) {
  ind <- match(3,sized$var1)
  print(sized$freq[ind])
}

maxno <- 150;
v <- 0:maxno;

allbacktracks <- data.frame(no=v, sized=numeric(maxno + 1), size500=numeric(maxno + 1), size1000=numeric(maxno + 1))

allbacktracks

for(i in 1:length(v)) {
  no <- allbacktracks[i,1]
  if (no %in% sized$var1) {
    ind <- match(no,sized$var1)
    allbacktracks$sized[i] <- sized$freq[ind]
  }
  
  if (no %in% size500$var1) {
    ind <- match(no,size500$var1)
    allbacktracks$size500[i] <- size500$freq[ind]
  }
  
  if (no %in% size1000$var1) {
    ind <- match(no,size1000$var1)
    allbacktracks$size1000[i] <- size1000$freq[ind]
  }
}

cbind(data[[1]], data2[[1]], data3[[1]])

cbind(data[[1]], data2[[1]], data3[[1]])[1,]

table(cbind(data[[1]], data2[[1]], data3[[1]]))

barplot(cbind(data[[1]], data2[[1]], data3[[1]]), 
        beside = TRUE)

allbacktracks <- data.frame(sized=table(data[[1]]), size500=table(data2[[1]]), size1000=table(data3[[1]]))

1:1000

cols <- c('red','blue', 'green');
ylim <- c(0, max(allbacktracks[c('sized','size500','size1000')])*1.2);
par(lwd=1);
barplot(
  t(allbacktracks[c('sized','size500','size1000')]),
  beside=T,
  ylim=ylim,
  border=cols,
  names.arg=allbacktracks$no,
  xlab='No of backtracks',
  ylab='Frequency',
  legend.text=c('sized','size500','size1000'),
  args.legend=list(text.col=cols,col=cols,border=cols,bty='n')
);
box();

df <- data.frame()

#agg <- aggregate(data, unique(data[1]), sum)
#agg
#
#plot(agg)
#
#typeof(data[1])
#data
#
#data <- 1
#unique(data[[1]])
#barplot(tapply(unique(data[[1]]), data[[1]], FUN=sum))

# 1557024587 sized with loops
# 1557025379 500   with loops
# 1557026849 1000  with loops

# 1557041683 sized backtrack
# 1557043141 500   backtrack
# 1557046598 1000  backtrack

loopdata <- read.csv('1557024587.sm_time', sep = ':')
barplot(table(loopdata[[2]]))
table(loopdata[[2]])

loop2data <- read.csv('1557025379.sm_time', sep = ':')
barplot(table(loop2data[[2]]))
table(loop2data[[2]])

loop3data <- read.csv('1557026849.sm_time', sep = ':')
barplot(table(loop3data[[2]]))
table(loop3data[[2]])

# 1557031926 sized without loops
# 1557033017 500   without loops
# 1557033867 1000  without loops

woloopdata <- read.csv('1557031926.sm_time', sep = ':')
table(woloopdata[[2]])

woloop2data <- read.csv('1557033017.sm_time', sep = ':')
table(woloop2data[[2]])

woloop3data <- read.csv('1557033867.sm_time', sep = ':')
table(woloop3data[[2]])


# 1557317720 sized backtrack opt
# 1557318886 500   backtrack opt
# 1557319918 1000  backtrack opt

backtrack_opt_sized <- read.table('1557317720.backtrack')
backtrack_opt_sized
mean(backtrack_opt_sized[[1]])
median(backtrack_opt_sized[[1]])
max(backtrack_opt_sized[[1]])
plot(table(backtrack_opt_sized[[1]]))

backtrack_opt_500 <- read.table('1557318886.backtrack')
backtrack_opt_500
mean(backtrack_opt_500[[1]])
median(backtrack_opt_500[[1]])
max(backtrack_opt_500[[1]])
plot(table(backtrack_opt_500[[1]]))

backtrack_opt_1000 <- read.table('1557319918.backtrack')
backtrack_opt_1000
mean(backtrack_opt_1000[[1]])
median(backtrack_opt_1000[[1]])
max(backtrack_opt_1000[[1]])
plot(table(backtrack_opt_1000[[1]]))


sized_opt <- data.frame(
  var1=as.numeric(as.vector(data.frame(table(backtrack_opt_sized[[1]]))$Var1)),
  freq=as.numeric(as.vector(data.frame(table(backtrack_opt_sized[[1]]))$Freq)));

size500_opt <- data.frame(
  var1=as.numeric(as.vector(data.frame(table(backtrack_opt_500[[1]]))$Var1)),
  freq=as.numeric(as.vector(data.frame(table(backtrack_opt_500[[1]]))$Freq)));

size1000_opt <- data.frame(
  var1=as.numeric(as.vector(data.frame(table(backtrack_opt_1000[[1]]))$Var1)),
  freq=as.numeric(as.vector(data.frame(table(backtrack_opt_1000[[1]]))$Freq)));

maxno_opt <- max(
  max(as.numeric(as.vector(data.frame(table(sized_opt[[1]]))$Var1))),
  max(as.numeric(as.vector(data.frame(table(size500_opt[[1]]))$Var1))),
  max(as.numeric(as.vector(data.frame(table(size1000_opt[[1]]))$Var1))));

maxno_opt <- 150;
v_opt <- 0:maxno_opt;

allbacktracks_opt <- data.frame(no=v_opt, sized=numeric(maxno_opt + 1), size500=numeric(maxno_opt + 1), size1000=numeric(maxno_opt + 1))

for(i in 1:length(v)) {
  no <- allbacktracks_opt[i,1]
  if (no %in% sized_opt$var1) {
    ind <- match(no,sized_opt$var1)
    allbacktracks_opt$sized[i] <- sized_opt$freq[ind]
  }
  
  if (no %in% size500_opt$var1) {
    ind <- match(no,size500_opt$var1)
    allbacktracks_opt$size500[i] <- size500_opt$freq[ind]
  }
  
  if (no %in% size1000_opt$var1) {
    ind <- match(no,size1000_opt$var1)
    allbacktracks_opt$size1000[i] <- size1000_opt$freq[ind]
  }
}

ylim <- c(0, max(allbacktracks_opt[c('sized','size500','size1000')])*1.2);
par(lwd=1);
barplot(
  t(allbacktracks_opt[c('sized','size500','size1000')]),
  beside=T,
  ylim=ylim,
  border=cols,
  col='white',
  names.arg=allbacktracks_opt$no,
  xlab='No of backtracks',
  ylab='Frequency',
  legend.text=c('sized','size500','size1000'),
  args.legend=list(text.col=cols,col=cols,border=cols,bty='n')
);
box();


# 1557024587 timeout evaluation
ch_data <- read.csv('1557024587.ch_time', sep = ':')
jsc_data <- read.csv('1557024587.jsc_time', sep = ':')
sm_data <- read.csv('1557024587.sm_time', sep = ':')
v8_data <- read.csv('1557024587.v8_time', sep = ':')
table(ch_data[[2]])

ch_data[[2]][552]
jsc_data[[2]][552]
sm_data[[2]][552]
v8_data[[2]][552]

max(ch_data[[2]])
max(jsc_data[[2]])
max(sm_data[[2]])
max(v8_data[[2]])

mean(ch_data[[2]])
mean(jsc_data[[2]])
mean(sm_data[[2]])
mean(v8_data[[2]])


plot(v8_data[[2]], type="l", col="red", lwd=1, xlab="Test #", ylab="Time (s)")
lines(jsc_data[[2]], type="l",col="blue")
lines(sm_data[[2]], type="l",col="orange")
lines(ch_data[[2]], type="l",col="green")
legend(900,10, c("v8","jsc","ch","sm"), col=c("red","blue","green","orange"), lty=1)
legend(900,10, c("v8","jsc"), col=c("red","blue"), lty=1)


# 1557251349 speed test
ch_data <- read.csv('1557251349.ch_time', sep = ':')
jsc_data <- read.csv('1557251349.jsc_time', sep = ':')
sm_data <- read.csv('1557251349.sm_time', sep = ':')
v8_data <- read.csv('1557251349.v8_time', sep = ':')
table(ch_data[[2]])

ch_data[[2]][999]
jsc_data[[2]][999]
sm_data[[2]][999]
v8_data[[2]][999]

max(ch_data[[2]])
max(jsc_data[[2]])
max(sm_data[[2]])
max(v8_data[[2]])

mean(ch_data[[2]])
mean(jsc_data[[2]])
mean(sm_data[[2]])
mean(v8_data[[2]])


plot(v8_data[[2]], type="l", col="red", lwd=1, xlab="# of elements (/10)", ylab="Time (s)")
lines(jsc_data[[2]], type="l",col="blue")
lines(sm_data[[2]], type="l",col="orange")
lines(ch_data[[2]], type="l",col="green")
legend(0,15, c("v8","jsc","ch","sm"), col=c("red","blue","green","orange"), lty=1)



# int_bount 10          10           -   1558966231
# int_bount 100         10           -   1558966515
# int_bount 1000        10           -   1558967307

stats_10_data <- read.csv('1558966231.stat.csv', sep = ';')
stats_10_data_backtrack <- read.table('1558966231.backtrack')
mean(stats_10_data[[7]])
mean(stats_10_data_backtrack[[1]])
median(stats_10_data[[7]])
max(table(stats_10_data[[7]]))
plot(table(stats_10_data[[7]]))
plot(table(stats_10_data_backtrack[[1]]))
mean(stats_10_data[[8]])


stats_100_data <- read.csv('1558966515.stat.csv', sep = ';')
stats_100_data_backtrack <- read.table('1558966515.backtrack')
mean(stats_100_data[[7]])
mean(stats_100_data_backtrack[[1]])
median(stats_100_data[[7]])
max(table(stats_100_data[[7]]))
plot(table(stats_100_data[[7]]))
plot(table(stats_100_data_backtrack[[1]]))
mean(stats_100_data[[8]])


stats_1000_data <- read.csv('1558967307.stat.csv', sep = ';')
stats_1000_data_backtrack <- read.table('1558967307.backtrack')
mean(stats_1000_data[[7]])
mean(stats_1000_data_backtrack[[1]])
median(stats_1000_data[[7]])
max(table(stats_1000_data[[7]]))
plot(table(stats_1000_data[[7]]))
table(stats_1000_data_backtrack[[1]])
plot(table(stats_10_data_backtrack[[1]]))
mean(stats_1000_data[[8]])


# tmp_ch_stack
# tmp_jsc_stack
# tmp_sm_stack
# tmp_v8_stack

ch_stack <- read.table('tmp_ch_stack')[[1]]
js_stack <- read.table('tmp_jsc_stack')[[1]]
sm_stack <- read.table('tmp_sm_stack')[[1]]
v8_stack <- read.table('tmp_v8_stack')[[1]]

plot(sm_stack, type="l")

sd(ch_stack)
sd(js_stack)
sd(sm_stack)
sd(v8_stack)

var(ch_stack)
var(js_stack)
var(sm_stack)
var(v8_stack)


mean(ch_stack)
mean(js_stack)
mean(sm_stack)
mean(v8_stack)


max(ch_stack)
max(js_stack)
max(sm_stack)
max(v8_stack)

min(ch_stack)
min(js_stack)
min(sm_stack)
min(v8_stack)

max(ch_stack) - min(ch_stack);
max(js_stack) - min(js_stack);
max(sm_stack) - min(sm_stack);
max(v8_stack) - min(v8_stack);

plot(ch_stack, type="l",col="green", lwd=1, xlab="Test #", ylab="# of lines", ylim=c(0, max(ch_stack)))
lines(v8_stack, type="l", col="red")
lines(js_stack, type="l",col="blue")
lines(sm_stack, type="l",col="orange")
legend(900,23000, c("ch","jsc","sm","v8"), col=c("green","blue","orange","red"), lty=1)
