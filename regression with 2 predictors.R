# regression with 2 predictor variables 

library(tidyverse)
library(plotly)

# make some random data for the heiught (t) of people based on their age (x1) and weight(x2)
x1_min <- 5 
x1_max <- 30
x2_min <- 40 
x2_max <- 75
x_n <- 20


# expand the model to predict height (t) from age (x1) and weight (x2)
# make some data
df <- tibble(x1　=　5 + 25 * runif(x_n))　%>%
	mutate(t = 170 - 108 * exp( -0.2 * x1) + 4 * rnorm(x_n)) %>%
	mutate(x2 = 23 * (t/100)^2 + 2 * rnorm(x_n))


plot_ly(x=df$x1, y=df$x2, z=df$t, type="scatter3d", mode="markers")

# Add Regression Plane to 3d Scatter Plot in Plotly
px1 = seq(x1_min, x1_max, 5)
px2 = seq(x2_min, x2_max, 5)
w <- c(1.5,1,90)
df_plane <- expand_grid(px1, px2) %>%
	mutate(py = w[1] * px1 + w[2] * px2 + w[3])

z = df_plane %>% 
	pivot_wider(names_from=px1, values_from=py) %>%
	select(-px2) %>%
	data.matrix()


plot_ly(x=px1, y=px2, z=z, type="surface") %>%
	add_trace(data=df, x=~x1, y=~x2, z=~t, mode="markers", type="scatter3d")


# calculate mean square error

mse_plane <- function(x1, x2, t, w1, w2, w3){
	y = w1 * x1 + w2 * x2 + w3
	mse = mean((y-t)^2)
	return(mse)
}

mse_plane(df$x1, df$x2, df$t, 1.5, 1, 90)

# calculates the gradient at w1 ,w2, and w2 (partial differential of mean square error J)
# J = 1/N ∑(y-t)^2
#   = 1/N ∑(w1*x1+w2*x2+w3-t)^2
# ∂J/∂w1 = 2/N ∑(w1*x1+w2*x2+w3-t)x1
#         = 2/N ∑(y-t)x1
# ∂J/∂w2 = 2/N ∑(w1*x1+w2*x2+w3-t)x2
#         = 2/N ∑(y-t)x2
# ∂J/∂w3 = 2/N ∑((w1*x1+w2*x2+w3-t))
#         = 2/N ∑(y-t)
dmse_plane <- function(x1, x2, t, w1, w2, w3){
	y = w1*x1 + w2*x2 + w3
	d_w1 = 2 * mean((y-t)*x1)
	d_w2 = 2 * mean((y-t)*x2)
	d_w3 = 2 * mean(y-t)
	return(c(d_w1, d_w2, d_w3))
}

# dmse_plane(df$x1, df$x2, df$t, 1.5, 1, 90)
# dmse_plane(df$x1, df$x2, df$t, 0.64, 1.015, 92)
w_init = c(1.5, 1, 90)
alpha = 0.00001
eps = 0.0001
i_max = 100000
# find w that minimize w numerically 
fit_plane_num <- function(x1, x2, t){
	w_i = matrix(data=0, nrow=i_max, ncol=3)
	w_i[1,] <- w_init
	for (i in 2:i_max){
		# calculate gradient
		dmse = dmse_plane(x1, x2, t, w_i[i-1, 1], w_i[i-1, 2], w_i[i-1, 3])
		print(paste("dmse", dmse))
		# update gradient
		w_i[i, 1] = w_i[i-1, 1] - alpha * dmse[1]
		w_i[i, 2] = w_i[i-1, 2] - alpha * dmse[2]
		w_i[i, 3] = w_i[i-1, 3] - alpha * dmse[3]
		print(paste("w:", w_i[i, ]))
		if (abs(dmse[1])<eps & abs(dmse[2])<eps & abs(dmse[3])<eps ) break
	}
	w1 = w_i[i, 1]
	w2 = w_i[i, 2]
	w3 = w_i[i, 3]
	w_i = w_i[1:i, ]
	colnames(w_i) <- c("w1", "w2", "w3")
	return(list(w1, w2, w3, w_i))
} 

plane_fit <- fit_plane_num(df$x1, df$x2, df$t)

history <- plane_fit[[4]] %>% as_tibble()

plot_ly(history, x=~w1, y=~w2, z=~w3, type="scatter3d", mode="lines")

#  this is the mse error of the estimate
mse_plane(df$x1, df$x2, df$t, plane_fit[[1]], plane_fit[[2]], plane_fit[[3]])


px1 = seq(x1_min, x1_max, 5)
px2 = seq(x2_min, x2_max, 5)
w <- c(plane_fit[[1]], plane_fit[[2]], plane_fit[[3]])
df_plane <- expand_grid(px1, px2) %>%
	mutate(py = w[1] * px1 + w[2] * px2 + w[3])

z = df_plane %>% 
	pivot_wider(names_from=px1, values_from=py) %>%
	select(-px2) %>%
	data.matrix()


plot_ly(x=px1, y=px2, z=z, type="surface") %>%
	add_trace(data=df, x=~x1, y=~x2, z=~t, mode="markers", type="scatter3d")
