#2.1.
x <- runif(100, 1, 100)
mean_x <- round(mean(x), 2)
x
?runif()

#Value changes as the 100 values are randomly distributed and therefore the mean changes
#after every iteration of the code. 

x
class(x)
mean(x)

#to get the same value every time, you can set a seed (which 'fixes' the randomization)


set.seed(12345)
x <- runif(100, 1, 100)
mean_x <- round(mean(x), 2)
x


#2.2

x <- (1:1000)
x_alt <- seq(1, 1000)

y <- seq(999, 0, by = 1)

z = x_alt + y
z
