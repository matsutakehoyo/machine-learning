library(tidyverse)
library(plotly)

# make some data for a classification problem. 
# Predictiong the sex of beatle (male or female) from its weight

x_min <- 0
x_max <- 2.5
x_n <- 60

# starting points of distribution
dist_s <- c(.4, .8)
# width of distribution
dist_w <- c(0.8, 1.6)


# probability of class 0
p <- .5
wk <- runif(x_n)
df <- tibble(t= 0 * (wk < p) + 1 * (wk >= p)) %>%
	mutate(x = runif(x_n) * dist_w[t+1] + dist_s[t+1])

ggplot(df) +
	geom_point(aes(x=x, y=t), alpha=1/3)

# assume 3 t=1 and 1 t=0 observations
# calculate the likelihood for different probabilities
# p(T=0,0,0,1|x) = w
# the maximum is w=0.25 is the most likely w given the data
tibble(w=seq(0,1,0.01)) %>%
	mutate(p=(1-w)^3 * (w)) %>%
	ggplot() +
		geom_line(aes(x=w, y=p))


# logistic function: naive inmplementation
logistic <- function(x, w){
	1/(1+exp(-w[1]*x-w[2]))
}

# numerical over thinking 
logistic_boundary <- function(w){
	# first sweep to home in
	x = seq(-100, 100, abs(1/w[1]))
	y = logistic(x, w)
	# loop until desired presicion is reached
	while((x[2]-x[1])>(0.001*abs(1/w[1]))){
		print(x[2]-x[1])
		x = seq(x[which(y<0.5) %>% max()], x[which(y>0.5) %>% min()], length.out=5)	
		y = logistic(x, w)
	}
	# (x[which(y<0.5) %>% max()]+x[which(y>0.5) %>% min()])/2
	x[which.min(abs(y-0.5))]
}

# simbple boundary
logistic_boundary <- function(w){
	-w[2]/w[1]
}
# w = c(1200,30)
# logistic_boundary(w)
# logistic(logistic_boundary(w), w)

# calculate mean cross entropy error (E(w)) of logistic funciton
# this involves very large numbers and easily results in over/underflow errors
cee_logistic <- function(.data, var_x, var_t, w){
	y = logistic(pull(.data, {{var_x}}), w)
	cee=0
	for (i in 1:nrow(.data)){
		t_i = pull(.data, {{var_t}})[i]
		# cee = cee - (t_i * log(y[i]) + (1-t_i) * log_sum_exp(log(1),-log(y[i])))
		cee = cee - (t_i * log(y[i]) + (1-t_i) * log(1-y[i]))
		# print(cee)
	}
	# divide by n to get the mean cee
	cee/nrow(.data)
} 

# Mächler, Martin (2012) “Accurately Computing log(1- exp(-|a|)) 
# How to Evaluate the Logistic Loss and not NaN trying
# http://fa.bianp.net/blog/2019/evaluate_logistic/
# https://cran.r-project.org/web/packages/Rmpfr/vignettes/log1mexp-note.pdf
logsig_log1pexp <- function(x){
	if (x< -33.3) x
	else if (x <= -18) x-exp(x)
	else if (x <= 37) -log1p(exp(-x))
	else -exp(-x)
}

# tibble(x=-100:10) %>% 
# 	mutate(y=map_dbl(x, ~logsig_log1pexp(.)), 
# 		   y2=map_dbl(x, ~logistic(., w=c(1,0)) %>% log())) %>%
# 	ggplot() + 
# 		geom_line(aes(x=x, y=y2, color='naive')) +
# 		geom_line(aes(x=x, y=y, color='log1pexp')) 

# use this formula to simplify: log(1−s(z))=−z+log(s(z))  
cee_logistic_stable <- function(.data, var_x, var_t, w){
	cee=0
	for (i in 1:nrow(.data)){
		t_i = pull(.data, {{var_t}})[i]
		x_i = pull(.data, {{var_x}})[i]
		log_y_i = logsig_log1pexp(w[1]*x_i+w[2])
		cee = cee - (t_i * log_y_i + (1-t_i) * (-(w[1]*x_i+w[2])+log_y_i))
	}
	# divide by n to get the mean cee
	cee/nrow(.data)
}

dcee_logistic <- function(.data, var_x, var_t, w){
	x = pull(.data, {{var_x}})
	t = pull(.data, {{var_t}})
	y = logistic(x, w)
	dcee = c(0,0)
	for (i in 1:nrow(.data)){
		dcee[1] = dcee[1] + (y[i] - t[i]) * x[i]
		dcee[0] = dcee[0] + (y[i] - t[i])
	}
	return(dcee)
}

dcee_logistic(df, x, t, c(1,1))

# optim is not very reliable for this function, maybe another method?
# also could not get optim to print estimates of at each iteration
out <- capture.output(w <- nlm(cee_logistic_stable, w<-c(0,0), .data=df, var_x=x, var_t=t, print.level=2)$estimate ) 

# optim(w<-c(0,0), cee_logistic_stable, gr=dcee_logistic, .data=df, var_x=x, var_t=t, method="CG", control=list(trace=1))

# parse output to extract the estimated parameters
trace <- out[grep("Parameter:", out) + 1] %>% 
	sub('\\[1\\] ', "", .) %>% 
	# negative lookahead
	# c(" 1 1") %>% sub(' (?!.*\\s)', ',', ., perl=TRUE)
	sub('\\s(?!.*\\s)', ',', ., perl=TRUE) %>%
	as.tibble() %>%
	separate(value, into=c('w1', 'w2'), sep=',', convert=TRUE)


cee_logistic_stable(df, x, t, w)
dcee_logistic(df, x, t, w)

tibble(x=seq(x_min, x_max, 0.01)) %>%
	mutate(y=logistic(x, w)) %>%
	ggplot(aes(x, y)) + geom_line() +
	geom_point(aes(x=x, y=t), df, alpha=1/3) +
	geom_vline(xintercept=logistic_boundary(w), color="deepskyblue")


# plot mean cross entropy error 
w1 = seq(-50,100, length.out=40)
w2 = seq(-100, 50, length.out=40)

df_cee <- expand_grid(w1=w1, w2=w2) %>%
	mutate(cee = map2_dbl(w1, w2, ~cee_logistic_stable(df, x, t, c(.x, .y))))


# contour plot
ggplot(df_cee, aes(x=w1, y=w2, z=cee)) +
	geom_contour_filled()

ggplot() +
	geom_contour(aes(x=w1, y=w2, z=cee), df_cee) +
	geom_line(aes(x=w1, y=w2), trace)

# 3d plot with plotly
z = df_cee %>% 
	select(w1, w2, cee) %>% 
	pivot_wider(names_from=w1, values_from=cee) %>%
	select(-w2) %>%
	data.matrix()
# plot_ly(df, x=~ww0, y=~ww1, z=~ff, color=~ff) %>% add_trace(type='mesh3d')
plot_ly(x=w1, y=w2, z=z) %>% 
	# add_surface() 
	add_surface(contours = list(z = list(show=TRUE,
		usecolormap=TRUE,
		highlightcolor="#ff0000",project=list(z=TRUE))))



logistic_boundary(w)

