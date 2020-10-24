library(tidyverse)
library(plotly)

# make data for 2 and 3 categories using 2 predictors
n = 100
k = 3

# mean and sd of the distribution
mu = rbind(c(-.5,-.5), c(.5,1), c(1,-.5))
sig = rbind(c(.7,.7), c(.8,.3), c(.3,.8))

# probability
p = c(.4, .8, 1)

df <- tibble(wk=runif(n)) %>%	
	mutate(t = ifelse(wk<p[1], 0, 1)) %>%
	mutate(t3 = case_when(
		wk<p[1] ~ 1,
		wk<p[2] ~ 2,
		TRUE ~ 3)) %>%
	mutate( x1= map_dbl(t3, ~rnorm(1) * sig[.,1]+mu[.,1]),
			x2= map_dbl(t3, ~rnorm(1) * sig[.,2]+mu[.,2]) )

ggplot(df, aes(x=x1, y=x2, color=as.factor(t))) +
	geom_point()

ggplot(df, aes(x=x1, y=x2, color=as.factor(t3))) +
	geom_point()

# logistic function with two predictors
logistic2 <- function(x1, x2, w){
	t = x1*w[1] + x2*w[2] + w[3]
	y = 1/(1+exp(-t))
	return(y)
}


# plot 2d logistic 
x1=seq(-3,3, length.out=50)
x2=seq(-3,3, length.out=50)
plane <- expand_grid(x1, x2) %>%
	mutate(y=logistic2(x1, x2, w=c(1,1,1)))

ggplot(plane, aes(x=x1, y=x2, z=y)) +
		# geom_contour()
		geom_contour_filled()

# 3d plot with plotly
z = plane %>% 
	pivot_wider(names_from=x1, values_from=y) %>%
	select(-x2) %>%
	data.matrix()

# boundary for logistic2 is a plane
# w1*[x1] + w2*[x2] = -w3
# b = expand_grid(x1, x2) %>%
# 	mutate( b = .5) %>%
# 	pivot_wider(names_from=x1, values_from=b) %>%
# 	select(-x2) %>%
# 	data.matrix()

plot_ly(showscale=FALSE) %>% 
	# add_surface() 
	add_surface(x=x1, y=x2, z=z,
		contours = list(z = list(show=TRUE,
		usecolormap=TRUE,
		highlightcolor="#ff0000",project=list(z=TRUE)))) 
	# add_surface(x=x1, y=x2, z=b, colorscale='Blues')

# to accuratetly esstimate log(logistic(x))
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

# cross entropy error = mean of log negative likelihood
# likelihood: t_n(1-y_n)+(1-t_n)y_n
# use this formula to simplify: log(1−s(z))=−z+log(s(z))  
cee_logistic2_stable <- function(.data, var_x1, var_x2, var_t, w){
	x1 = pull(.data, {{var_x1}})
	x2 = pull(.data, {{var_x2}})
	t = pull(.data, {{var_t}})
	cee=0
	for (i in 1:nrow(.data)){
		log_y_i = logsig_log1pexp(w[1]*x1[i]+w[2]*x2[i]+w[3])
		cee = cee - (t[i] * log_y_i + (1-t[i]) * (-(w[1]*x1[i]+w[2]*x2[i])+log_y_i))
	}
	# divide by n to get the mean cee
	cee/nrow(.data)
}

# cross entropy
cee_logistic2_stable(df, x1, x2, t, w=c(1,1,1))

# take a look at the topology of cee_logistic2
# takes too long... or maybe there is a coding error
# w1 = seq(-50,50, length.out=10)
# w2 = seq(-50, 50, length.out=10)
# w3 = seq(-50, 50, length.out=10)

# df_cee <- expand_grid(w1=w1, w2=w2, w3=w3) %>%
# 	mutate(cee = pmap_dbl(list(w1, w2, w3), ~cee_logistic2_stable(df, x1, x2, t2, c(..1, ..2,..3))))


# density gradient
dcee_logistic2 <- function(.data, var_x1, var_x2, var_t, w){
	x1 = pull(.data, {{var_x1}})
	x2 = pull(.data, {{var_x2}})
	t = pull(.data, {{var_t}})
	y = logistic2(x1, x2, w)
	dcee = c(0,0,0)
	for (i in 1:nrow(.data)){
		dcee[1] = dcee[1] + (y[i] - t[i]) * x1[i]
		dcee[2] = dcee[2] + (y[i] - t[i]) * x2[i]
		dcee[3] = dcee[3] + (y[i] - t[i])
	}
	dcee = dcee/nrow(.data)
	return(dcee)
}

# dcee_logistic2(df, x1, x2, t, c(1,1,1))


# w <- optim(w<-c(0,0,0), cee_logistic2_stable, gr=dcee_logistic2, 
# 	.data=df, var_x1=x1, var_x2=x2, var_t=t2, method="CG", control=list(trace=2))

out <- capture.output(w <- optim(w<-c(0,0,0), cee_logistic2_stable, gr=dcee_logistic2, 
	.data=df, var_x1=x1, var_x2=x2, var_t=t, method="CG", control=list(trace=1))$par)

trace <- out[grep("parameters", out)] %>% 
    gsub('[ ]{2,}', ' ', .) %>%
	sub('parameters ', "", .) %>% 
	sub('\\s(?! *\\s)', ',', ., perl=TRUE) %>%
	sub('\\s(?! *\\s)', ',', ., perl=TRUE) %>%
	as_tibble() %>%
	separate(value, into=c('w1', 'w2', 'w3'), sep=',', convert=TRUE)


# visualise the optimization
plot_ly() %>% 
	add_markers(x=trace$w1, y=trace$w2, z=trace$w3) %>%
	add_lines(x=trace$w1, y=trace$w2, z=trace$w3)

# check cross entropy error and its gradient
cee_logistic2_stable(df, x1, x2, t, w)
dcee_logistic2(df, x1, x2, t, w)


x1=seq(-3,3, length.out=50)
x2=seq(-3,3, length.out=50)
plane <- expand_grid(x1, x2) %>%
	mutate(y=logistic2(x1, x2, w=w))

z_plane = plane %>% 
	pivot_wider(names_from=x1, values_from=y) %>%
	select(-x2) %>%
	data.matrix()

z = seq(0,1, length.out=50 )

plot_ly() %>% 
	# add_surface() 
	add_surface(x=x1, y=x2, z=z_plane,
		contours = list(z = list(show=TRUE,
		usecolormap=TRUE,
		highlightcolor="#ff0000",project=list(z=TRUE)))) %>%
	add_markers(x=df$x1, y=df$x2, z=df$t, marker=list(color='gray'))  

# # the boundry
# w1*x1+w2*x2+w3=0
# x2 = -w1/w2*x1 - w3/w2
ggplot() +
	# geom_contour(aes(x=x1, y=x2, z=y), plane)
	geom_contour_filled(aes(x=x1, y=x2, z=y), plane) +
	geom_abline(slope=-w[1]/w[2], intercept=-w[3]/w[2], color='red') +
	scale_fill_grey() +
	geom_point(aes(x=x1, y=x2, color=as.factor(t)), df)

