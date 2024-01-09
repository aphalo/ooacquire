library(ggplot2)
packageVersion("ggplot2")

set.seed(1)
df <- data.frame(x = 1:100, y = rnorm(100) * 1e-5)

ggplot(df, aes(x, y)) + geom_point()
