doQQ
====

R function to create simple QQ plots, including shaded confidence intervals and lambda value.

## Inputs/options
* p              ::  {REQUIRED} vector of P-values
* main           ::  {optional} title of plot
* subtitle       ::  {optional} subtitle of plot
* outFile        ::  {optional} file name of PNG graphic to produce
* outFile.width  ::  {optional} {default=700} width (pixels) of PNG graphic to produce
* outFile.height ::  {optional} {default=500} height (pixels) of PNG graphic to produce

## Example
```
nTests <- 5e5

p1 <- runif(nTests)
p2 <- p1^2

png("example.doQQ.png", width=1000, height=800)
par(mfrow=c(2,2))

hist(p1)
doQQ(p1, main="Random uniform distribution", subtitle="50,000 tests")

hist(p2)
doQQ(p2, main="Random uniform distribution, with inflation", subtitle="50,000 tests")

dev.off()
```
![](http://s29.postimg.org/xktgfsr4n/example_do_QQ.png)
