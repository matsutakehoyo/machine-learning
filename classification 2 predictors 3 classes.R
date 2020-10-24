library(tidyverse)
library(plotly)

# make data for 2 and 3 categories using 2 predictors
n = 200
k = 3

# mean and sd of the distribution
mu = rbind(c(-.5,-.5), c(.5,1), c(1,-.5))
sig = rbind(c(.7,.7), c(.8,.3), c(.3,.8))

# probability
p = c(.4, .8, 1)
k = length(p)

df <- tibble(wk=runif(n)) %>%	
	# mutate(t = ifelse(wk<p[1], 0, 1)) %>%
	mutate(t = case_when(
		wk<p[1] ~ 1,
		wk<p[2] ~ 2,
		TRUE ~ 3)) %>%
	mutate( x1= map_dbl(t, ~rnorm(1) * sig[.,1]+mu[.,1]),
			x2= map_dbl(t, ~rnorm(1) * sig[.,2]+mu[.,2]) )

ggplot(df, aes(x=x1, y=x2, color=as.factor(t))) +
	geom_point()



# softmax function with two predictors, returns three values
# pass w as 1d vector, because optim does not work on matrices
# this version is not vectorised for x1 and x2
logistic3 <- function(x1, x2, w){
	# a_k = w1_k*x1 + w2_k*x2 + w3_k
	# u = ∑exp(a1)
	w_m=matrix(w, nrow=3, ncol=3) %>% t()
	u=0
	a=c(0,0,0)
	for (i in 1:3){
		a[i] <- w_m[i,1]*x1+w_m[i,2]*x2+w_m[i,3]
		u = u + exp(a[i])
	}
	return(exp(a)/u)
}

# w <- 1:9
# logistic3(2,2, w)

cee_logistic3 <- function(.data, var_x1, var_x2, var_t, w){
	x1 = pull(.data, {{var_x1}})
	x2 = pull(.data, {{var_x2}})
	t = pull(.data, {{var_t}})
	cee=0
	for (i in 1:nrow(.data)){
		y_i = logistic3(x1[i], x2[i], w)
		cee = cee - log(y_i[t[i]])
	}
	# divide by n to get the mean cee
	cee/nrow(.data)
}

# cee_logistic3(df, x1, x2, t, w)

dcee_logistic3 <- function(.data, var_x1, var_x2, var_t, w){
	x1 = pull(.data, {{var_x1}})
	x2 = pull(.data, {{var_x2}})
	t = pull(.data, {{var_t}})
	dcee = rbind(c(0,0,0),c(0,0,0),c(0,0,0))
	for (i in 1:nrow(.data)){
		y_i = logistic3(x1[i], x2[i], w)
		y_i = logistic3(df$x1[i], df$x2[i], w)
		# as.integer(t[i]==k) zero unless k is t
		for (k in 1:3){
			dcee[k,] = dcee[k,] + (y_i[k] - as.integer(t[i]==k)) * c(x1[i],x2[i],1)
		}

	}
	dcee = dcee/nrow(.data)
	return(as.vector(t(dcee)))
}


# optim(w<-rep(0,9), cee_logistic3, gr=dcee_logistic3, 
# 	.data=df, var_x1=x1, var_x2=x2, var_t=t, method="CG", control=list(trace=2))

out <- capture.output(w <- optim(w<-rep(0,9), cee_logistic3, gr=dcee_logistic3, 
	.data=df, var_x1=x1, var_x2=x2, var_t=t, method="CG", control=list(trace=1, maxit=500))$par)

# check cross entropy error and its gradient
cee_logistic3(df, x1, x2, t, w)
dcee_logistic3(df, x1, x2, t, w)

# output is split in two lines, first lines has w1-w7 and the second w8 and w9
# first line
trace1 <- out[grep("parameters", out)] %>% 
	sub('parameters', "", .) %>% 
	sub('^([ ]{2,})', '',.) %>%
	sub('([ ]{1,})$', '',.) %>%
	gsub('[ ]{1,}', ',',.) %>%
	as_tibble() %>%
	separate(value, into=paste0('w', 1:7), sep=',', convert=TRUE)
# second line
trace2 <- out[grep("parameters", out)+1] %>% 
	sub('^([ ]{2,})', '',.) %>%
	sub('([ ]{1,})$', '',.) %>%
	gsub('[ ]{1,}', ',',.) %>%
	as_tibble() %>%
	separate(value, into=paste0('w', 8:9), sep=',', convert=TRUE)

trace <- bind_cols(trace1, trace2) %>% rowid_to_column('iter') %>%
	pivot_longer(
		cols=starts_with("w"),
		names_to="parameter", 
		values_to='value')

# visualise the optimization
ggplot(trace, aes(x=iter, y=value, color=parameter)) + 
	geom_line()

# visualize boundary with countour plot
x1=seq(-3,3, length.out=50)
x2=seq(-3,3, length.out=50)
plane <- expand_grid(x1, x2) %>%
	mutate(y=map2(x1, x2, ~data.frame(logistic3(.x, .y, w=w)%>%t()) %>% setNames(c('y1', 'y2', 'y3')))) %>%
	unnest(y) 

w_m <- matrix(w, nrow=3, ncol=3) %>% t()
ggplot() +
	geom_contour(aes(x=x1, y=x2, z=y1), plane, breaks=c(0.5, 0.9)) +
	geom_contour(aes(x=x1, y=x2, z=y2), plane, breaks=c(0.5, 0.9)) +
	geom_contour(aes(x=x1, y=x2, z=y3), plane, breaks=c(0.5, 0.9)) +
	geom_point(aes(x=x1, y=x2, color=as.factor(t)), df)




# 3D visualization
planes <- list()
for (i in 1:k){
	# i=2
	planes[[i]] = plane %>% 
		select(x1, x2, .data[[paste0('y',i)]]) %>%
		pivot_wider(names_from=x1, values_from=.data[[paste0('y',i)]]) %>%
		select(-x2) %>%
		data.matrix()
}
plot_ly() %>% 
	add_surface(x=x1, y=x2, z=planes[[1]],
		colorscale = 'Hot') %>%
	add_surface(x=x1, y=x2, z=planes[[2]],
		colorscale = 'Greens') %>%
	add_surface(x=x1, y=x2, z=planes[[3]],
		colorscale = 'Blues') %>%
	add_markers(x=df$x1, y=df$x2, z=.5, 
	        color=df$t,  # set color to an array/list of desired values
	        opacity=0.8 )  

