##########################################
### Principal Component Analysis (PCA) ###
##########################################

library(ggfortify)

# PCA with iris dataset
iris.df <- iris
head(iris.df)

# creating another dataframe from iris dataset that contains the columns from 1 to 4
iris.X <- iris.df[,1:4]
iris.X

principal_components <- princomp(iris.X, cor = TRUE, score = TRUE)

summary(principal_components)


# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# loadings
principal_components$loadings

