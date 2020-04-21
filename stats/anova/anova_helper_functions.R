

{
  if (FALSE) # Test data
  {
    means = c(3, 4, 7)
    n_within = c(3, 2, 5)
    sds = 3.4
  }
  
  
  # Construct a data.table with randomly generated, normally-distributed, data.
  # 'means' is a vector of group means.
  # 'sds' is a vector of group standard deviations.  It must either be the same length as 'means' or a single number.
  # 'n_within' specifies the number of replicates within a group.  It can be either a single number or a vector the same length as 'means'
  # 'seed' allows to provide a random seed
  random_one_way_anova_data = function(means, sds, n_within, seed = NA)
  {
    if (!is.na(seed)) set.seed(seed)
    
    if (length(n_within) > 1) 
    {
      stopifnot(length(means) == length(n_within)) 
      data_mean = rep(means, n_within)
      factor = rep(1:length(means), n_within)
    } else
    {
      data_mean = rep(means, each = n_within)
    }
    
    if (length(sds) > 1)
    {
      stopifnot(length(means) == length(sds)) else
        sds = rep(sds, n_within)
    } else
    {
      data_sd = 
    }
    
    n_groups = length(means)
    means_1 = rep(means, each = n_within)
    if (length(sds) == 0) sds = rep(sds, n_groups)
    sds_1 = rep(sds, each = n_within)
    
    dat = data.table(
      mean = means_1,
      sd = sds_1,
      y = rnorm(length(means_1), means_1, sds_1),
      group = rep(1:n_groups, each = n_within),
      name = rep(LETTERS[1:n_groups], each = n_within))
    
    return(dat)
  }
}


{
  
  if (FALSE)
  {
    factor_1_effects = c(1, 4)
    factor_2_effects = 3
    base_case_mean = 2.3
    
    cells = 
      data.frame(
        n        = c(4, 4, 4, 4, 4, 4),
        factor_1 = c(1, 1, 2, 2, 3, 3),
        factor_2 = c(1, 2, 1, 2, 1, 2))
    
    
    dat_sim = sim.anova(es1 = 3, es2 = 5, n1 = 2, n2 = 2, n3 = 0, n = 5, factors = T)
    
    head(dat_sim)
    names(dat_sim) = c("f1", "f2", "i_1_2", "y")
    
    fit1 = lm(y ~ f1 + f2, data = dat_sim)
    
    summary(fit1)
    anova(lm(y ~ f1 + f2, data = dat_sim))
    
    
    
    data.df <- sim.anova(es1=1,es2=.5,es13=1)  # one main effect and one interaction
    describe(data.df)
    pairs.panels(data.df)
    
    
    require(psych)
    sim.anova()
    
    
    random_anova_data = function(effects_1, effects_2, base_case, cells)
    {
      
    }
    
  }
}


set.seed(42)
data.df <- sim.anova(es1=1,es2=.5,es13=1)  # one main effect and one interaction
describe(data.df)
pairs.panels(data.df)   #show how the design variables are orthogonal
#
summary(lm(DV~IV1*IV2*IV3,data=data.df))
summary(aov(DV~IV1*IV2*IV3,data=data.df))




# Set coefficients
alpha = 10
beta1 = .3
beta2 = -.5
beta3 = -1.1

n_reps = 5

std = 2
std = c(1, 2, 2, 1, 2, 2)
n_1 = 2; n_2 = 3

coeffs = c(3, 4, 4, 3, 4)


anova_simple_two_way_data = function(
  alpha, coeffs,
  n_reps, n_1, n_2, 
  std = 1, seed = NULL)
{
  if (!is.null(seed)) set.seed(seed)
  n_groups = n_1 * n_2
  i = n_1 - 1; j = n_2 - 1
  n_interactions = i * j
  
  N = n_reps * n_groups
  n_coeffs = i + j + n_interactions
  
  stopifnot(length(std) == 1 | length(std) == n_groups)
  stopifnot(length(coeffs) == n_coeffs)
  
  factor_dat = data.frame(x1 = gl(n_1, n_reps, N), x2 = gl(n_2, n_reps, N))
  design_m = model.matrix(~ x1 * x2, factor_dat)
  errors = rnorm(n = N, sd = rep(std, each = n_reps))
  
  dat_out = cbind(factor_dat, y = design_m %*% c(alpha, coeffs) + errors)
  return(dat_out)
}
  

dat_1 = anova_simple_two_way_data(
  n_1 = 2, n_2 = 3,
  alpha = 12, 
  c(2.4, 1.2, 5.4, 0, 0),
  n_reps = 5)

fit1 = lm(y ~ x1 * x2, data = dat_1)
fit1 = lm(y ~ x1 + x2, data = dat_1)
summary(fit1)

# Generate 200 trials
A = c(rep(c(0), 100), rep(c(1), 100)) # '0' 100 times, '1' 100 times
B = rep(c(rep(c(0), 50), rep(c(1), 50)), 2) # '0'x50, '1'x50, '0'x50, '1'x50
e = rnorm(200, 0, sd=1) # Random noise, with standard deviation of 1

# Generate your data using the regression equation
y = alpha + beta1*A + beta2*B + beta3*A*B + e

# Join the variables in a data frame
data = data.frame(cbind(A, B, y))

# Fit an ANOVA
fit1 = lm(y ~ A * B, data = data)
summary(fit1)
model = aov(y ~ A*B, data=data)
summary(model)

str(fit1)
predict.lm(
  
)

