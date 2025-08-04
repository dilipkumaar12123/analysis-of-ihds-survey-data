library(foreign)
library(dplyr)
d <- read.dta("C:\\Users\\Hayathi\\Desktop\\statsdf.dta")
df <- d %>% filter(State != 5)
134517 - 1268   #133249

df$in_educ <- as.factor(df$in_educ)
df$age_grp <- as.factor(df$age_grp)

#1
prop.table(table(df$in_educ))[2]
table(df$in_educ)

#2a
prop.table(table(df$gender[df$in_educ == 1]))

#2b
#df$age_grp <- factor(df$age_grp, levels = 0:4, 
#       labels = c("below 15", "15-17yrs", "18-21yrs", "22-25yrs", "26-29yrs"))

joint_table <- table(df$age_grp, df$gender, df$in_educ)

#joint probabilities
joint_p <- prop.table(joint_table)
joint_p
#marginal probabilities
marginal_age <- prop.table(margin.table(joint_table, 1))
marginal_age
marginal_gender <- prop.table(margin.table(joint_table, 2))
marginal_gender
marginal_in_educ <- prop.table(margin.table(joint_table, 3))
marginal_in_educ

#joint and marginal
ftable(addmargins(joint_p, margin = c(1, 2), FUN = sum))

#3
df1 <- df %>% filter(time_noncurri > 0)
q3 <- table(df1$mpce, df1$time_noncurri)
sum(prop.table(q3))

#3a
library(ggplot2)
ggplot(aes("", mpce), data = df1) +
  geom_boxplot()

ggplot(aes("", time_noncurri), data = df1) +
  geom_boxplot()

#3b
library(psych)
describe(df1[, c("mpce", "time_noncurri")])[, c("mean", "median", "sd", "skew", "kurtosis")]

mode_fn <- function(x) {
  x <- x[!is.na(x) & x != 0]
  tbl <- table(x)
  modes <- as.numeric(names(tbl)[tbl == max(tbl)])
  return(modes)
}
mode_fn(df1$mpce)
mode_fn(df1$time_noncurri)

#3c
chebyshev <- function(x,y) {
  x <- x[!is.na(x) & x != 0]
  y <- y[!is.na(y) & y != 0]
  mean_ci <- mean(x, na.rm = TRUE)
  sd_ci <- sd(x, na.rm = TRUE)
  lower_bound <- mean_ci - (y * sd_ci)
  upper_bound <- mean_ci + (y * sd_ci)
  z1 <- (lower_bound - mean_ci)/sd_ci
  z2 <- (upper_bound - mean_ci)/sd_ci
  result <- pnorm(z2) - pnorm(z1)
  print(lower_bound)
  print(upper_bound)
  print(result)
}

chebyshev(df1$mpce,1)
chebyshev(df1$mpce,2)
chebyshev(df1$mpce,3)

#3d


df1 <- df1 %>%
  mutate(quintile = ntile(mpce, 5))
stats <- df1 %>%
  group_by(quintile) %>%
  summarise(
    mean_mpce = mean(mpce, na.rm = TRUE),
    var_mpce = var(mpce, na.rm = TRUE),
    sd_mpce = sd(mpce, na.rm = TRUE),
    mean_time_noncurri = mean(time_noncurri, na.rm = TRUE),
    var_time_noncurri = var(time_noncurri, na.rm = TRUE),
    sd_time_noncurri = sd(time_noncurri, na.rm = TRUE),
    correlation = cor(mpce, time_noncurri)
  )

# View the results
print(stats)
cor(df1$mpce,df1$time_noncurri, use = "complete.obs")
var(df1$mpce,df1$time_noncurri, use = "complete.obs")

#4
#4a
df1$lnmpce <- log(df1$mpce)
df1$lntncur <- log(df1$time_noncurri)

describe(df1[, c("lnmpce", "lntncur")])[, c("mean", "median", "sd", "skew", "kurtosis")]
mode_fn(df1$lnmpce)
mode_fn(df1$lntncur)

chebyshev(df1$lnmpce,1)
chebyshev(df1$lnmpce,2)
chebyshev(df1$lnmpce,3)


#4b
library(plotly)
library(MASS)

df_clean <- df[is.finite(df$lnmpce) & is.finite(df$lntncur), ]

x <- seq(min(df_clean$lnmpce), max(df_clean$lnmpce), length.out = 100)
y <- seq(min(df_clean$lntncur), max(df_clean$lntncur), length.out = 100)
density <- kde2d(df_clean$lnmpce, df_clean$lntncur, n = 100, lims = c(range(x), range(y)))

fig <- plot_ly(
  x = density$x, y = density$y, z = density$z,
  type = "surface",
  colorscale = "Viridis"
) %>%
  layout(
    title = "Bivariate Normal Distribution of lnmpce and lntncur",
    scene = list(
      xaxis = list(title = "lnmpce"),
      yaxis = list(title = "lntncur"),
      zaxis = list(title = "Density")
    )
  )

fig

#4c
rural <- df %>%
  filter(Sector == "Rural 1")

mean_x <- mean(rural$mpce, na.rm = TRUE)
mean_x
var_x <- var(rural$mpce, na.rm = TRUE)
var_x

urban <- df %>%
  filter(Sector == "Urban 2")

mean_y = mean(urban$mpce, na.rm = TRUE)
mean_y
var_y = var(urban$mpce, na.rm = TRUE)
var_y

cov_xy <- cor(rural$mpce, urban$mpce, na.rm = TRUE)
cov_xy

#4d
# Load necessary library
library(ggplot2)
set.seed(123)

# Draw random samples
sample_lnmpce_10 <- df1$lnmpce[sample(1:nrow(df1), 10)]
sample_lnmpce_50 <- df1$lnmpce[sample(1:nrow(df1), 50)]
sample_lnmpce_100 <- df1$lnmpce[sample(1:nrow(df1), 100)]

sample_in_educ_10 <- df1$in_educ[sample(1:nrow(df1), 10)]
sample_in_educ_50 <- df1$in_educ[sample(1:nrow(df1), 50)]
sample_in_educ_100 <- df1$in_educ[sample(1:nrow(df1), 100)]

# Histogram of lnmpce for sample size 10
ggplot(data.frame(sample_lnmpce_10), aes(x = sample_lnmpce_10)) +
  geom_histogram(bins = 10) +
  geom_density(color = "red") +
  labs(title = "Histogram of lnmpce (Sample size: 10)", x = "lnmpce", y = "Frequency")

# Histogram of lnmpce for sample size 50
ggplot(data.frame(sample_lnmpce_50), aes(x = sample_lnmpce_50)) +
  geom_histogram(bins = 10) +
  geom_density(color = "red") +
  labs(title = "Histogram of lnmpce (Sample size: 50)", x = "lnmpce", y = "Frequency")

# Histogram of lnmpce for sample size 100
ggplot(data.frame(sample_lnmpce_100), aes(x = sample_lnmpce_100)) +
  geom_histogram(bins = 10) +
  geom_density(color = "red") +
  labs(title = "Histogram of lnmpce (Sample size: 100)", x = "lnmpce", y = "Frequency")

# Q-Q plot of in_educ for sample size 10
ggplot(data.frame(sample_in_educ_10), aes(sample = as.numeric(sample_in_educ_10))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Q-Q Plot of in_educ (Sample size: 10)", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Q-Q plot of in_educ for sample size 50
ggplot(data.frame(sample_in_educ_50), aes(sample = as.numeric(sample_in_educ_50))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Q-Q Plot of in_educ (Sample size: 50)", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Q-Q plot of in_educ for sample size 100
ggplot(data.frame(sample_in_educ_100), aes(sample = as.numeric(sample_in_educ_100))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Q-Q Plot of in_educ (Sample size: 100)", x = "Theoretical Quantiles", y = "Sample Quantiles")


#5
library(sf)
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
map <- st_read("C:\\Users\\Hayathi\\Downloads\\map")                 #spatial data
map$distid <- as.numeric(map$dtcode11)
map_codes <- read.dta("C:\\Users\\Hayathi\\Desktop\\map codes shp.dta")

map_df <- map_codes %>%
  left_join(map, by = "distid") %>%
  filter(State == 5) %>%
  mutate(district = as.numeric(district))

mapdf <- d %>%
  filter(State == 5) %>%
  dplyr::select(State, state, mpce, district, in_educ, age) %>%
  mutate(district = as.numeric(district))

map_data <- mapdf %>%
  left_join(map_df, by = "district")


map_ineduc <- map_data %>%
  filter(in_educ == 1, age <= 25)

map_poor <- map_data %>%
  mutate(quintile = ntile(mpce, 5)) %>%
  filter(quintile %in% c(1,2))

# Plot the map using ggplot2
ggplot(data = map_ineduc) +
  geom_sf(aes(fill = in_educ),color = "black", size = 0.1) +
  labs(title = "District Map - in education") +
  scale_fill_viridis_c()
  theme_minimal()

  ggplot(data = map_poor) +
    geom_sf(aes(fill = quintile),color = "black", size = 0.1) +
    labs(title = "District Map - poor quintile") +
    scale_fill_viridis_c()
  theme_minimal()

#6
q6 <- df %>%
  dplyr::select(totaltime_sna, totaltime_esna, totaltime_nsna, total_time_1_8) %>%
  mutate(
    sh_sna = totaltime_sna/total_time_1_8,
    sh_esna = totaltime_esna/total_time_1_8,
    sh_nsna = totaltime_nsna/total_time_1_8
  )

q6$sh_sna <- pmax(q6$sh_sna, 0)  # Truncate at zero for sh_sna
q6$sh_esna <- pmax(q6$sh_esna, 0)  # Truncate at zero for sh_esna
q6$sh_nsna <- pmin(q6$sh_nsna, 1)  # Truncate at one for sh_nsna  

ggplot(aes(sh_sna),data = q6) +
  geom_histogram()
ggplot(aes(sh_esna),data = q6) +
  geom_histogram()
ggplot(aes(sh_nsna),data = q6) +
  geom_histogram()
