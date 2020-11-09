library(tidyverse)

# make data for 3 categories using 2 predictors
n = 200
k = 3

# mean and sd of the distribution
mu = rbind(c(-.5,-.5), c(.5,1), c(1,-.5))
sig = rbind(c(.7,.7), c(.8,.3), c(.3,.8))

# probability
p = c(.4, .8, 1)

df = tibble(wk=runif(n)) %>%	
	mutate(t = case_when(
		wk<p[1] ~ 1,
		wk<p[2] ~ 2,
		TRUE ~ 3)) %>%
	mutate( x1= map_dbl(t, ~rnorm(1) * sig[.,1]+mu[.,1]),
			x2= map_dbl(t, ~rnorm(1) * sig[.,2]+mu[.,2]) )

ggplot(df, aes(x=x1, y=x2, color=as.factor(t))) +
	geom_point()

test_ratio = 0.5
df = df %>% mutate(data=ifelse(runif(n)>test_ratio, 'train', 'test'))

ggplot(df, aes(x=x1, y=x2, color=as.factor(t))) +
	geom_point() +
	facet_grid(.~data)

# logistic function: naive inmplementation
sigmoid = function(x){
	1/(1+exp(-x))
}

soft_max = function(a){
	k = length(a)
	u=0
	for (i in 1:k){
		u = u + exp(a[i])
	}
	return(exp(a)/u)
}
# ggplot(data.frame(x=c(-5,5)), aes(x=x)) +
# 	stat_function(fun=sigmoid)


# feed-forward neural netwrok
# w is a d+1 x m matrix and v is a m+1 x k matrix
fnn = function(x, w, v){
	#  x: matrix with d predictors for n data points (n x d)
	n = dim(x)[1]
	# d is the number of inputs
	d = dim(x)[2]
	#  m: the number of neurons in the middle layer
	m = length(w)/(d+1)
	#  k: the numnber of output categories
	k = length(v)/(m+1)
	# add dummy input to x for constant term
	x = cbind(x, rep(1,n))
	# convert w to a matrix
	#  w: the weights of input to m nodes a m x (d+1) 
	# w_ji is the weight for the input i to j
	w_m = matrix(w, nrow=d+1, ncol=m) %>% t()
	#  b: the sum of inputs from x (d+1)
	# b_j = ∑w_ji x_i
	b = matrix(0, nrow=n, ncol=m)
	#  z: the output of the middle layer sigmoid(a) (d+1)
	z = matrix(0, nrow=n, ncol=m)
	for (i in 1:n){
		b[i,] = w_m %*% x[i,]
		z[i,] = sigmoid(b[i,])
	}	
	# add dummy input to z for constant term 
	z = cbind(z, rep(1,n))
	#  v: the weigths of middle layer to the output b  k x (m + 1)
	v_m = matrix(v, nrow=m+1, ncol=k) %>% t()
	#  a: the sum of inputs from z 
	a = matrix(0, nrow=n, ncol=k)
	#  y: the probability of each of the k categories
	y = matrix(0, nrow=n, ncol=k)
	for (i in 1:n){
		a[i,] = v_m %*% z[i,] 
		y[i,] = soft_max(a[i,])
	}				
	return(list(b=b, z=z, a=a, y=y))
}

# x = rbind(c(-0.14, 0.87), c(-0.87,-1.25))
# fnn(x[1:2,], w=rep(1,6), v=rep(1,9))

# cross entropy error of FNN
# assume predictors start with x and categories stored in t in .data
cee_fnn = function(.data, w, v){
	t = pull(.data, t)
	x = .data %>% select(starts_with('x')) %>% as.matrix()
	y = fnn(x, w, v)$y
	cee=0
	for (i in 1:nrow(.data)){
		cee = cee - log(y[i, t[i]])
	}
	# divide by n to get the mean cee
	cee/nrow(.data)
}

# cee_fnn(df, rep(1,6), rep(1,9))

# gradient of cross entropy error of FNN at w and v
# assume predictors start with x and categories stored in t in .data
dcee_fnn_num = function(.data, w, v, epsilon=0.01){
	par = c(w,v)
	d_par = rep(NA,length(par))
	# loop parameters
	for (i in 1:length(par)){
		p_epsilon = par
		p_epsilon[i] = par[i] + epsilon
		mse1 = cee_fnn(.data, 
					   p_epsilon[1:length(w)], 
					   p_epsilon[(length(w)+1):length(par)])
		p_epsilon = par
		p_epsilon[i] = par[i] - epsilon
		mse2 = cee_fnn(.data, 
					   p_epsilon[1:length(w)], 
					   p_epsilon[(length(w)+1):length(par)])
		d_par[i] = (mse1 - mse2)/(2*epsilon)
	}
	d_w = d_par[1:length(w)]
	d_v = d_par[(length(w)+1):length(par)]
	return(list(d_w=d_w, d_v=d_v))
}

dcee_fnn = function(.data, w, v){
	x = .data %>% select(starts_with('x')) %>% as.matrix()
	#  x: matrix with d predictors for n data points (n x d)
	n = dim(x)[1]
	# d is the number of inputs
	d = dim(x)[2]
	#  m: the number of neurons in the middle layer
	m = length(w)/(d+1)
	#  k: the numnber of output categories
	k = length(v)/(m+1)
	t = pull(.data, t)
	# y is a n x k matrix
	y = fnn(x, w, v)$y 
	z = fnn(x, w, v)$z 
	# b = fnn(x, w, v)$b 
	# convert w to a matrix
	#  w: the weights of input to m nodes a m x (d+1) 
	# w_ji is the weight for the input i to j
	w = matrix(w, nrow=d+1, ncol=m) %>% t()
	d_w = matrix(0, nrow=d+1, ncol=m) %>% t()

	#  v: the weigths of middle layer to the output b  k x (m + 1)
	v = matrix(v, nrow=m+1, ncol=k) %>% t()
	d_v = matrix(0, nrow=m+1, ncol=k) %>% t()
	# loop parameters
	for (i in 1:n){
		# dekta2 = (y_k - t_k)
		delta2 = y[i,]
		delta2[t[i]] = delta2[t[i]] - 1
		# ∂E/∂v_kj = delta2_k * z_j
		for (ki in 1:k)
			d_v[ki,] = d_v[ki,] + delta2[ki] * z[i,]
		# calculate d_w		
		delta1 = rep(0,m)
		# delta1 = (1-z_j) * z_j * ∑ (v_kj * delta2_k)
		# ∂E/∂w_ji = delta1_j * x
		for (j in 1:m){
			delta1[j] = (1-z[i,j]) * z[i,j] * (v[,j] %*% delta2)
			d_w[j,] = d_w[j,] + delta1[j] * c(x[i,],1)
		}

	}
	d_v = d_v/n
	d_w = d_w/n
	
	d_v = d_v %>% t() %>% as.vector()
	d_w = d_w %>% t() %>% as.vector()
	return(list(d_w=d_w, d_v=d_v))
}

w=rnorm(6)
# w = 1:6
v=rnorm(9)
# v = 1:9
dcee_fnn_num(df, w, v)
dcee_fnn(df, w, v)

# .train and .test are data.frames
fit_dcee_fnn = function(.train, .test, 
	w_init, v_init, alpha=0.5, iter=1000){
	w = w_init
	v = v_init
	df_cee <- tibble(cee_train=cee_fnn(.train, w_init, v_init),
					 cee_test=cee_fnn(.test, w_init, v_init))
	df_trace = c(w, v)	
	for (i in 1:iter){
		par <- dcee_fnn(.train, w, v)
		# update w and v
		# w_ji(t+1) = v_ji(t) - alpha * ∂E/∂w_ji
		w = w - alpha * par$d_w
		# v_kj(t+1) = v_kj(t) - alpha * ∂E/∂v_kj
		v = v - alpha * par$d_v
		df_trace = rbind(df_trace, c(w, v))
		df_cee = df_cee %>% add_row(cee_train=cee_fnn(.train, w, v),cee_test=cee_fnn(.test, w, v))
	}
	df_trace = as_tibble(df_trace, .name_repair='unique')
	colnames(df_trace) = c(paste0('w', 1:length(w)), paste0('v', 1:length(v)))
	return(list(df_trace=df_trace, df_cee=df_cee, w=w, v=v))
}

# fit FNN with two neurons in the middle layer
fit <- fit_dcee_fnn(filter(df, data=='train'), filter(df, data=='test'), 
			w_init=rnorm(6,0,.01), v_init=rnorm(9,0,.01), iter=1000, alpha=.5)


# fit_num <- fit_dcee_fnn_num(filter(df, data=='train'), filter(df, data=='test'), 
# 			w_init=rnorm(6,0,.01), v_init=rnorm(9,0,.01))

df_cee = fit$df_cee %>% 
	rowid_to_column('iter') %>%
	pivot_longer(-iter)

ggplot(df_cee) +
	geom_line(aes(x=iter, y=value, color=name))

df_trace = fit$df_trace %>% 
	rowid_to_column('iter') %>%
	pivot_longer(-iter)

ggplot(df_trace) +
	geom_line(aes(x=iter, y=value, color=name))

# visualize the results
x1=seq(-3,3, length.out=50)
x2=seq(-3,3, length.out=50)
plane <- expand_grid(x1, x2) %>%
	# sample_n(10) %>%
	bind_cols(fnn(as.matrix(.), fit$w, fit$v)$y %>% 
			as_tibble %>% setNames(c('y1', 'y2', 'y3')))

ggplot() +
	geom_contour(aes(x=x1, y=x2, z=y1), plane, breaks=c(0.5, 0.9)) +
	geom_contour(aes(x=x1, y=x2, z=y2), plane, breaks=c(0.5, 0.9)) +
	geom_contour(aes(x=x1, y=x2, z=y3), plane, breaks=c(0.5, 0.9)) +
	geom_point(aes(x=x1, y=x2, color=as.factor(t)), df)

# visualize the trained FNN
library(plotly)

b = expand_grid(x1, x2) %>%
	as.matrix() %>%
	fnn(., fit$w, fit$v) %>%
	.$b

b1 = matrix(b[,1], nrow=50, ncol=50)
b2 = matrix(b[,2], nrow=50, ncol=50)

z = expand_grid(x1, x2) %>%
	as.matrix() %>%
	fnn(., fit$w, fit$v) %>%
	.$z

z1 = matrix(z[,1], nrow=50, ncol=50)
z2 = matrix(z[,2], nrow=50, ncol=50)

a = expand_grid(x1, x2) %>%
	as.matrix() %>%
	fnn(., fit$w, fit$v) %>%
	.$a

a1 = matrix(a[,1], nrow=50, ncol=50)
a2 = matrix(a[,2], nrow=50, ncol=50)
a3 = matrix(a[,3], nrow=50, ncol=50)

y = expand_grid(x1, x2) %>%
	as.matrix() %>%
	fnn(., fit$w, fit$v) %>%
	.$y

y1 = matrix(y[,1], nrow=50, ncol=50)
y2 = matrix(y[,2], nrow=50, ncol=50)
y3 = matrix(y[,3], nrow=50, ncol=50)

fig1 = plot_ly(x=x1, y=x2, z=~b1, scene='scene1') 
fig1 = fig1 %>% add_surface(showscale=FALSE)

fig2 = plot_ly(x=x1, y=x2, z=~z1, scene='scene2') 
fig2 = fig2 %>% add_surface(showscale=FALSE)

fig3 = plot_ly(x=x1, y=x2, z=~a1, scene='scene3') 
fig3 = fig3 %>% add_surface(showscale=FALSE)

fig4 = plot_ly(x=x1, y=x2, z=~y1, scene='scene4') 
fig4 = fig4 %>% add_surface(showscale=FALSE)

fig5 = plot_ly(x=x1, y=x2, z=~b2, scene='scene5') 
fig5 = fig5 %>% add_surface(showscale=FALSE)

fig6 = plot_ly(x=x1, y=x2, z=~z2, scene='scene6') 
fig6 = fig6 %>% add_surface(showscale=FALSE)

fig7 = plot_ly(x=x1, y=x2, z=~a2, scene='scene7') 
fig7 = fig7 %>% add_surface(showscale=FALSE)

fig8 = plot_ly(x=x1, y=x2, z=~y2, scene='scene8') 
fig8 = fig8 %>% add_surface(showscale=FALSE)

fig9 = plot_ly(x=x1, y=x2, z=~a3, scene='scene9') 
fig9 = fig9 %>% add_surface(showscale=FALSE)

fig10 = plot_ly(x=x1, y=x2, z=~y3, scene='scene10') 
fig10 = fig10 %>% add_surface(showscale=FALSE)


fig = subplot(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9, fig10)

# custom grid style
axx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)'
)

fig = fig %>% layout(title = "FNN",
         scene = list(domain=list(x=c(0,0.25),y=c(0.5,0.83)),
                      xaxis=axx, yaxis=axx, zaxis=axx,
                      aspectmode='cube'),
         scene2 = list(domain=list(x=c(0.25,0.5),y=c(0.5,0.83)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene3 = list(domain=list(x=c(0.5,0.75),y=c(0.66,1)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene4 = list(domain=list(x=c(0.75,1),y=c(0.66,1)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene5 = list(domain=list(x=c(0,0.25),y=c(0.17,0.5)),
                      xaxis=axx, yaxis=axx, zaxis=axx,
                      aspectmode='cube'),
         scene6 = list(domain=list(x=c(0.25,0.5),y=c(0.17,0.5)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene7 = list(domain=list(x=c(0.5,0.75),y=c(0.33,0.66)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene8 = list(domain=list(x=c(0.75,1),y=c(0.33,0.66)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene9 = list(domain=list(x=c(0.5,0.75),y=c(0,0.33)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene10 = list(domain=list(x=c(0.75,1),y=c(0,0.33)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'))

fig

