# regression with 1 predictor variable 

library(tidyverse)
library(plotly)

# make some random data for the heiught of people based on their age
x_min <- 4 
x_max <- 30
x_n <- 16

Prm_c <- c(170, 108, 0.2)


df <- tibble(x　=　5 + 25 * runif(x_n))　%>%
	mutate(T = 170 - 108 * exp( -0.2 * x) + 4 * rnorm(x_n))

df %>% ggplot(aes(x=x, y=T)) + geom_point()

# first approximate this data with a straight line 
# y = w_1 * x + w_2
# using the mean square error J

mse_line <- function(x, t, w_1, w_2){
	y = w_1 * x + w_2
	mse = mean((y-t)^2)
	return(mse)
}

xn <- 100
w1_range <- c(-25, 25)
w2_range <- c(120, 170)

x1 = seq(w1_range[1], w1_range[2], length.out=xn)
x2 = seq(w2_range[1], w2_range[2], length.out=xn)

df_mse_lin <- 
	expand_grid(x1=x1, x2=x2) %>%
	mutate(J = map2_dbl(x1, x2, ~mse_line(df$x, df$T, .x, .y)))

df_mse_lin %>% 
	ggplot(aes(x=x1, y=x2, z=J)) + geom_contour_filled()
		# geom_contour(breaks=c(100, 1000, 10000, 100000))

# 3d plot with plotly

z = df_mse_lin %>% 
	pivot_wider(names_from=x1, values_from=J) %>%
	select(-x2) %>%
	data.matrix()
# plot_ly(df, x=~ww0, y=~ww1, z=~ff, color=~ff) %>% add_trace(type='mesh3d')
plot_ly(x=x1, y=x2, z=z) %>% 
	# add_surface() 
	add_surface(contours = list(z = list(show=TRUE,
		usecolormap=TRUE,
		highlightcolor="#ff0000",project=list(z=TRUE))))

# use steepest descent to calculate the w_1 and w_2 that minimize the square erro J
# start in a random position and calculate the gradient and move a little bit to the oposite direction
# the amount to move is alpha
# evaluate and do the same in the new position untill the gradient is arbitrarily small

# this calculates the gradient at w_1 and w_2 (partial differential of mean square error J)
# J = 1/N ∑(y-t)^2
#   = 1/N ∑(w_1*x+w_2-t)^2
# ∂J/∂w_1 = 2/N ∑(w_1*x+w_2-t)x
#         = 2/N ∑(y-t)x
# ∂J/∂w_2 = 2/N ∑((w_1*x+w_2-t))
#         = 2/N ∑(y-t)
dmse_line <- function(x, t, w_1, w_2){
	y = w_1 * x + w_2
	d_w1 = 2 * mean((y-t)*x)
	d_w2 = 2 * mean(y-t)
	return(c(d_w1, d_w2))
}

dmse_line(df$x, df$T, 10, 165)

w_init = c(-10, 165)
alpha = 0.01
eps = 0.0001
i_max = 100000

fit_line_num <- function(x, t){
	w_i = matrix(data=0, nrow=i_max, ncol=2)
	w_i[1,] <- w_init
	for (i in 2:i_max){
		dmse = dmse_lin(x, t, w_i[i-1, 1], w_i[i-1, 2])
		w_i[i, 1] = w_i[i-1, 1] - alpha * dmse[1]
		w_i[i, 2] = w_i[i-1, 2] - alpha * dmse[2]
		if (abs(dmse[1]) < eps & abs(dmse[2]) < eps ) break
	}
	w1 = w_i[i, 1]
	w2 = w_i[i, 2]
	w_i = w_i[1:i, ]
	colnames(w_i) <- c("w1", "w2")
	return(list(w1, w2, w_i))
} 

line_fit <- fit_line_num(df$x, df$T)

history <- line_fit[[3]] %>% as_tibble()

ggplot() + 
	geom_point(aes(x=x, y=T), data=df) + 
	geom_abline(slope=line_fit[[1]], intercept=line_fit[[2]])


ggplot() + 
	geom_contour_filled(aes(x=x1, y=x2, z=J), data=df_mse_lin) +
	geom_point(aes(x=w1, y=w2), data=history, size=1)

