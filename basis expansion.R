# expand the linear regression 
# use a basis function phi(x) instead of x
#  use a gauss function for this example
library(tidyverse)

# make some random data for the heiught (t) of people based on their age (x)
x_min <- 4 
x_max <- 30
x_n <- 16

df <- tibble(x　=　5 + 25 * runif(x_n))　%>%
	mutate(t = 170 - 108 * exp( -0.2 * x) + 4 * rnorm(x_n))

df %>% ggplot(aes(x=x, y=t)) + geom_point()

# define the gauss function
gauss <- function(x, mu, s){
	exp(-(x-mu)^2/(2*s^2))
}

tibble(x=seq(-5,5, length.out=100)) %>% 
	mutate(y=gauss(x, 0, 1)) %>%
	ggplot() +
		geom_line(aes(x=x, y=y))

# use M=4 gauss basis functions
m=4
mu = seq(5, 30, length.out=m)
s = mu[2] - mu[1]

df_gauss <- tibble(x=seq(x_min, x_max, length.out=100)) 

for (i in 1:m){
	df_gauss <- mutate(df_gauss, !!paste0("phi", i):=gauss(x, mu[i], s))
}

df_gauss %>% 
	pivot_longer(-x) %>%
	ggplot() + geom_line(aes(x=x, y=value, color=name))

# for example when weights are w = -2, 3, -1, 2, 10
w <- c(-2, 3, -1, 2, 10)

gauss_func <- function(w, x){
	m = length(w)-1 # -1 because the last w is 1
	mu = seq(5, 30, length.out=m)
	s = mu[2] - mu[1]
	y = rep(0, length(x))
	for (i in 1:m){
		y = y + w[i] * gauss(x, mu[i], s)
	}
	y = y + w[m+1]
	return(y)
}

tibble(x = seq(x_min, x_max, length.out=100)) %>%
	mutate(y = gauss_func(w, x)) %>%
	ggplot() +
		geom_line(aes(x=x, y=y))

# calculate mse given w
mse_gauss_func <- function(.data, var_x, var_t, w){
	y = gauss_func(w, pull(.data, {{var_x}}))
	mse = (y-pull(.data, {{var_t}}))^2 %>% mean()
	return(mse)
}

# mse_gauss_func(df, x, t, w)

# find the weights for the basis function analytically
fit_gauss_func <- function(.data, var_x, var_t, m){
	mu = seq(5, 30, length.out=m)
	s = mu[2] - mu[1]
	n = nrow(.data)
	psi = matrix(1, nrow=n, ncol=m+1)
	for(i in 1:m){
		# i=1
		psi[,i] = gauss(pull(.data, {{var_x}}), mu[i], s)
	}
	psi_T = t(psi)
	b = psi_T %*% psi %>% solve() 
	c = b %*% psi_T 
	# w = c %*% t
	w = c %*% pull(.data, {{var_t}})
	return(as.vector(w))
}

w <- fit_gauss_func(df, x, t, 3)
# w <- c(30, 75, 2.9, 99, 55)

# make the gauss function using the weights in w
make_gauss <- function(w){
	m = length(w) - 1
	mu = seq(5, 30, length.out=m)
	s = mu[2] - mu[1]
	df_gauss <- tibble(x=seq(x_min, x_max, length.out=100)) 

	for (i in 1:m){
		# i=1
		df_gauss <- mutate(df_gauss, !!paste0("phi_", i):=gauss(x, mu[i], s))
	}
	df_gauss <- mutate(df_gauss, !!paste0("phi_", m+1):=1)
	
	df_gauss <- mutate(df_gauss, y=1)
	for (i in 1:(m+1)){
		phi <- sym(paste0("phi_", i))
		df_gauss <- df_gauss %>%
			mutate(y = y + !!phi*w[i])
	}
	df_gauss %>% 
		select(-starts_with("phi")) %>%
		mutate(m=m) 
		# %>%
		# mutate(mse=mse_gauss_func(df$x, df$T, w), 
		# 	sd=sqrt(mse))
}

w <- fit_gauss_func(df, x, t, 4) 
mse <- mse_gauss_func(df, x, t, w)

make_gauss(w) %>%
	ggplot() + geom_line(aes(x=x, y=y)) +
	geom_point(aes(x=x, y=t), df) +
	geom_text(aes(x = 10, y=180, label = paste0("sd=", round(sqrt(mse),1))), stat='unique')

# use 2-10 gauss functions more gauss reduces sd
fit_gauss <- map(2:10, ~fit_gauss_func(df, x, t, .) %>% 
		make_gauss()) %>% bind_rows()
mse <- map(2:10, ~fit_gauss_func(df, x, t, .) %>% 
					mse_gauss_func(df, x, t, .) %>% 
					tibble(mse=., m=.x)) %>%
	bind_rows() %>%
	mutate(sd = sqrt(mse))

ggplot(fit_gauss) + geom_line(aes(x=x, y=y)) +
	geom_point(aes(x=x, y=t), df) +
	geom_text(aes(x = 15, y=190, label = paste0("sd=", round(sd,1))), mse) +
	facet_wrap(~m) +
	ylim(130, 200)

# larger M resuls in overfitting use 1/4 of the data for testing to estimate the best m
# spli the data into training and testing
df <- df %>% 
	mutate(data=ifelse(row_number()<as.integer(x_n/4), 'test', 'train'))

fit_gauss <- map(2:10, ~fit_gauss_func(filter(df,data=='train'), x, t, .) %>% 
		make_gauss()) %>% bind_rows()

mse_train <- map(2:10, ~fit_gauss_func(filter(df,data=='train'), x, t, .) %>% 
					mse_gauss_func(filter(df,data=='train'), x, t, .) %>% 
					tibble(mse=., m=.x, data='train')) %>%
	bind_rows() %>%
	mutate(sd = sqrt(mse))

mse_test <- map(2:10, ~fit_gauss_func(filter(df,data=='train'), x, t, .) %>% 
					mse_gauss_func(filter(df,data=='test'), x, t, .) %>% 
					tibble(mse=., m=.x, data='test')) %>%
	bind_rows() %>%
	mutate(sd = sqrt(mse))


ggplot(fit_gauss) + geom_line(aes(x=x, y=y)) +
	geom_point(aes(x=x, y=t, color=data), df) +
	geom_text(aes(x = 15, y=190, label = paste0("train sd=", round(sd,1))), mse_train) +
	geom_text(aes(x = 20, y=140, label = paste0("test sd=", round(sd,1))), mse_test) +
	facet_wrap(~m) +
	ylim(130, 200)

full_join(mse_train, mse_test) %>%
	ggplot() +
		geom_line(aes(x=m, y=sd, color=data))


# the results are highly dependent on the data order use K hold cross validatiaon 

# separate data into k sets for training and testing
make_validation_data <- function(df, k){
	n <- nrow(df)
	for (i in 1:k){
		df <- df %>% 
			mutate(!!paste0("validation", i):=ifelse(row_number()%%k==i-1, 'test', 'train'))			
	}
	return(df)
}

k=16
d_val <- make_validation_data(df, k)
w <- list(); mse_train <-list(); mse_test <-list()
for (i in 1:k){
	# i=1
	d_val <- d_val %>% mutate(data = !!sym(paste0('validation', i)))

	w[[i]] <- map(2:10, ~tibble(w=fit_gauss_func(filter(d_val,data=='train'), x, t, .), 
								m=.x) %>% mutate(id = row_number())) %>% bind_rows()

	mse_train[[i]] <- map(2:10, ~fit_gauss_func(filter(d_val,data=='train'), x, t, .) %>% 
					mse_gauss_func(filter(d_val,data=='train'), x, t, .) %>% 
					tibble(mse=., m=.x, data='train', validation=i)) %>%
		bind_rows() %>%
		mutate(sd = sqrt(mse))

	mse_test[[i]] <- map(2:10, ~fit_gauss_func(filter(d_val,data=='train'), x, t, .) %>% 
					mse_gauss_func(filter(d_val, data=='test'), x, t, .) %>% 
					tibble(mse=., m=.x, data='test', validation=i)) %>%
		bind_rows() %>%
		mutate(sd = sqrt(mse))	
}

bind_rows(mse_train, mse_test) %>%
	group_by(m, data) %>%
	summarise(mse=mean(mse), 
		      sd=mean(sd)) %>%
	ggplot() +
		geom_line(aes(x=m, y=sd, color=data)) +
		geom_point(aes(x=m, y=sd, color=data))

# m=2-4 shows the lowest sd in train and test data sets. Use this m on all the data
w <- fit_gauss_func(df, x, t, 3) 
mse <- mse_gauss_func(df, x, t, w)

make_gauss(w) %>%
	ggplot() + geom_line(aes(x=x, y=y)) +
	geom_point(aes(x=x, y=t), df) +
	geom_text(aes(x = 10, y=180, label = paste0("sd=", round(sqrt(mse),1))), stat='unique')
