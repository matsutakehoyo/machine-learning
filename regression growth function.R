# parametric model
# use a growth curve to fit the data
library(tidyverse)

# make some random data for the heiught (t) of people based on their age (x)
x_min <- 4 
x_max <- 30
x_n <- 16

df <- tibble(x　=　5 + 25 * runif(x_n))　%>%
	mutate(t = 170 - 108 * exp( -0.2 * x) + 4 * rnorm(x_n))
df %>% ggplot(aes(x=x, y=t)) + geom_point()

# model_a is a growth curve
model_a <- function(x, w){
	y = w[1] - w[2] * exp(-w[3] * x)
}

tibble(x=0:100) %>%
	mutate(y = model_a(x, c(150,100,.05))) %>%
	ggplot(aes(x=x, y=y)) + geom_line()

# mse for parametric model given w
mse_model_a <- function(.data, var_x, var_t, w){
	y = model_a(pull(.data, {{var_x}}), w)
	mse = (y-pull(.data, {{var_t}}))^2 %>% mean()
	return(mse)
}


# optimise w usign nlm
w <- nlm(mse_model_a, w <- c(180,100,0.1), .data=df, var_x=x, var_t=t)$estimate
# optimise w usign optim
w <- optim(par=c(170,100,0.3), mse_model_a, .data=df, var_x=x, var_t=t)$par

tibble(x=seq(x_min, x_max, length.out=100)) %>%
	mutate( y = model_a(x, w)) %>%
	ggplot() +
		geom_line(aes(x=x, y=y)) +
		geom_point(aes(x=x, y=t), df)

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

	w[[i]] <- optim(par=c(180,100,0.1), mse_model_a, .data=filter(d_val, data=='train'), var_x=x, var_t=t)$par

	mse_train[[i]] <- mse_model_a(filter(d_val, data=='train'), x, t, w[[i]]) %>% 
		tibble(mse=., data='train', validation=i) %>%
		mutate(sd = sqrt(mse))

	mse_test[[i]] <- mse_model_a(filter(d_val, data=='test'), x, t, w[[i]]) %>% 
		tibble(mse=., data='test', validation=i) %>%
		mutate(sd = sqrt(mse))	
}

mse_a <- bind_rows(mse_train, mse_test) %>% 
	group_by(data) %>%
	summarise(mse=mean(mse), 
		      sd=mean(sd))
ggplot(mse_a) +
	geom_point(aes(x='model_a', y=sd, color=data)) 



# comapre with basis expansion
# define the gauss function
gauss <- function(x, mu, s){
	exp(-(x-mu)^2/(2*s^2))
}

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

k=16
d_val <- make_validation_data(df, k)
w_gauss <- list(); mse_train_gauss <-list(); mse_test_gauss <-list()
for (i in 1:k){
	# i=1
	d_val <- d_val %>% mutate(data = !!sym(paste0('validation', i)))

	w_gauss[[i]] <- map(2:10, ~tibble(w=fit_gauss_func(filter(d_val,data=='train'), x, t, .), 
								m=.x) %>% mutate(id = row_number())) %>% bind_rows()

	mse_train_gauss[[i]] <- map(2:10, ~fit_gauss_func(filter(d_val,data=='train'), x, t, .) %>% 
					mse_gauss_func(filter(d_val,data=='train'), x, t, .) %>% 
					tibble(mse=., m=.x, data='train', validation=i)) %>%
		bind_rows() %>%
		mutate(sd = sqrt(mse))

	mse_test_gauss[[i]] <- map(2:10, ~fit_gauss_func(filter(d_val,data=='train'), x, t, .) %>% 
					mse_gauss_func(filter(d_val, data=='test'), x, t, .) %>% 
					tibble(mse=., m=.x, data='test', validation=i)) %>%
		bind_rows() %>%
		mutate(sd = sqrt(mse))	
}

mse_gauss <- bind_rows(mse_train_gauss, mse_test_gauss) %>%
	group_by(m, data) %>%
	summarise(mse=mean(mse), 
		      sd=mean(sd)) 

mse_a %>% add_column(m='exp') %>% 
	full_join(., mse_gauss %>% mutate(m=as.character(m))) %>%
	ggplot() +
		geom_point(aes(x=m, y=sd, color=data)) 

