set.seed(123)

rnormtrunc <- function(n,mean,sd){
	b <- rep(0,n)
	flag<-rep(TRUE,n)
	while(any(flag)){
		b[flag] <- rnorm(n=sum(flag), mean=mean, sd=sd)
		flag<- (b<0) | (b>1)
	}
	return(b)
}

n <- 50
total <- rep(200,n)
mu.b <- 0.5
mu.w <- 0.5


bb <- rnormtrunc(n, mean=mu.b, sd=0.2)
bw <- rnormtrunc(n, mean=mu.w, sd=0.2)

x <- round(runif(n) * total)
notx <- total - x

z11 <- round(bb*x)
z21 <- round(bw*notx)
y <- z11 + z21
noty <- total - y
x.frac <- x/total
y.frac <- y/total
notx.frac <- notx/total
noty.frac <- noty/total

mydata <- data.frame(bb,bw,z11,z21,x,notx,x.frac,notx.frac,y,noty,y.frac,noty.frac,total)

# Different possible formula representations:

z.out <- zeiml$new()
z.out$zelig(y.frac~x.frac, N=total, data=mydata)
expect_that(length(z.out$getcoef()[[1]]), equals(25))
expect_that(round(z.out$getcoef()[[1]]$phi), equals(c(0,0,-2,-2,0,0,0)))

z2.out <- zeiml$new()
z2.out$zelig(y.frac~x.frac, N="total", data=mydata)
expect_that(length(z2.out$getcoef()[[1]]), equals(25))
expect_that(round(z2.out$getcoef()[[1]]$phi), equals(c(0,0,-2,-2,0,0,0)))

z3.out <- zeiml$new()
z3.out$zelig( cbind(y.frac,noty.frac)~cbind(x.frac,notx.frac), N="total", data=mydata)
expect_that(length(z3.out$getcoef()[[1]]), equals(25))
expect_that(round(z3.out$getcoef()[[1]]$phi), equals(c(0,0,-2,-2,0,0,0)))

z4.out <- zeiml$new()
z4.out$zelig(y~x, N="total", data=mydata)
expect_that(length(z4.out$getcoef()[[1]]), equals(25))
expect_that(round(z4.out$getcoef()[[1]]$phi), equals(c(0,0,-2,-2,0,0,0)))

