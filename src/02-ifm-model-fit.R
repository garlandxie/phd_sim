# function was adapted from Graham et al. 2015. Biol Conserv
# https://doi.org/10.1016/j.biocon.2015.09.002
# see also https://github.com/laurajanegraham/ifm_r/blob/master/R/model.fit.R

# prep -------------------------------------------------------------------------

# create a matrix to hold parameters x, y, and u
params <- matrix(0, nrow = 10, ncol=4)

# assign variables -------------------------------------------------------------

# get calculated patch sizes
area <- survey$area_m2 

# get latitude coordinates of a given patch
x_crd <- survey$x_coords 

# get longitude coordinates of a given patch 
y_crd <- survey$y_coords

# estimate strength of the distance decay from empirical dispersal kernels
# using mean seed diplacement distance
# e.g., Biswas et al. 2015. Biol Inv for Garlic Mustard
alpha <- 1.82

# calculate interpatch distances 
# flexible approach; can include edge-edge distances and wind direction?
d <- dist(cbind(x_crd, y_crd), method = "euclidean") 

# calculate connectivity -------------------------------------------------------

# for IFM, connectivity (S) is the sum of each patch
# where the migration of each patch is the product of patch size, occupancy, 
# and interpatch distance (weighted by dispersal distance) 
edis <- as.matrix(exp(-alpha*d))
diag(edis) <- 0
edis <- sweep(edis, 2, A, "*")

# this is for a single snapshot of occupancies 
# but could include multiple surveys from GBIF?
p <- survey[,5:(no.survey+4)]
P <- rowSums(p)
S <- rowSums(edis[, p > 0])

# estimate parameters ----------------------------------------------------------

# run logistic regression 
# incidence function model can be parameterized as a linear model of 
# log-odds incidence
mod <- glm(P ~ offset(2*log(S)) + log(A), family = binomial(link = "logit"))

# estimate parameters x 
# x represents the extent to which a species’ survival is dependent on patch
# size (larger x represents weaker dependence)
beta <- coef(mod)
x <- beta[2]

# estimate parameters y and u from log(uy)
# assume that the smallest plot where the
# species was present is of the size where extinction probability E = 1
A0 <- min(A[P>0])
ey <- exp(-beta[1])
e <- A0^x
y <- sqrt(ey/e) 

# summarize parameters ---------------------------------------------------------
params <- data.frame(x = x, e = e, y = y) 