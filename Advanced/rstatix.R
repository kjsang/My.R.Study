install.packages("rstatix")
install.packages("ggpubr")
library(rstatix)
library(ggpubr)  
install.packages("zip")

iris %>%
  get_summary_stats(Sepal.Length, Sepal.Width, type = "common")

iris %>% get_summary_stats(type = "common")

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
iris %>%
  group_by(Species) %>%
  get_summary_stats(Sepal.Length, type = "mean_sd")

df <- ToothGrowth
df$dose <- as.factor(df$dose)
head(df)
df %>% t_test(len ~ 1, mu = 0)

df %>%
  group_by(dose) %>%
  t_test(len ~ 1, mu = 0)

# T-test
stat.test <- df %>%
  t_test(len ~ supp, paired = FALSE)
stat.test
#> # A tibble: 1 x 8
#>   .y.   group1 group2    n1    n2 statistic    df      p
#> * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>  <dbl>
#> 1 len   OJ     VC        30    30      1.92  55.3 0.0606

# Create a box plot
p <- ggboxplot(
  df, x = "supp", y = "len",
  color = "supp", palette = "jco", ylim = c(0,40)
)
# Add the p-value manually
p + stat_pvalue_manual(stat.test, label = "p", y.position = 35)
install.packages("d")
citation("rstatix")

install.packages("btergm")

install.packages("remotes")
library("remotes")
install_github("leifeld/dna/rDNA@v2.0-beta.24", INSTALL_opts = "--no-multiarch")

library(rDNA)
?rDNA
??rDNA
install.packages("rJava")
library("rJava")
