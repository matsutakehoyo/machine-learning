library(tidyverse)
library(plotly)

# a 2d function 
f <- function(w0, w1){
	return(w0^2 + 2*w0*w1 + 3)
}

# partial differential for w0
df_dw0 <- function(w0, w1){
	return(2*w0 + 2*w1)
}

# partial differential for w1
df_dw1 <- function(w0, w1){
	return(2*w0)
}

w_range = 2
dw = 0.25
w0 = seq(-w_range, w_range, dw)
w1 = seq(-w_range, w_range, dw)

# f is surface in 3d space
# contour plot
df <- tibble(ww0 = w0) %>%
	mutate(ww1 = list(w1)) %>%
	unnest(ww1) %>%
	mutate(ff = f(ww0, ww1), 
		dff_dw0 = df_dw0(ww0, ww1), 
		dff_dw1 = df_dw1(ww0, ww1))

ggplot(df, aes(x=ww0, y=ww1, z=ff)) +
	# geom_contour()
	geom_contour_filled()

# 3d plot with plotly
z = df %>% 
	select(ww0, ww1, ff) %>% 
	pivot_wider(names_from=ww0, values_from=ff) %>%
	select(-ww1) %>%
	data.matrix()
# plot_ly(df, x=~ww0, y=~ww1, z=~ff, color=~ff) %>% add_trace(type='mesh3d')
plot_ly(x=w0, y=w1, z=z) %>% 
	# add_surface() 
	add_surface(contours = list(z = list(show=TRUE,
		usecolormap=TRUE,
		highlightcolor="#ff0000",project=list(z=TRUE))))

# quiver plot show which direction the gradient points 
# the minima is at the oposite direction of the gradient 
library(ggquiver)

ggplot(df) +
	aes(x=ww0, y=ww1) +		
	# geom_point()
	geom_quiver(aes(u=dff_dw0, v=dff_dw1))

