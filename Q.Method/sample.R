install.packages("qmethod")
# also installing the dependencies ‘tmvnsim’, ‘sass’, ‘jquerylib’, ‘mnormt’, ‘httpuv’, ‘sourcetools’, ‘bslib’, ‘psych’, ‘GPArotation’, ‘xtable’, ‘shiny’, ‘rjson’
library(qmethod)
# This is 'qmethod' v.1.8.
# 
# Please cite as:
#   Zabala, A. (2014) qmethod: A Package to Explore Human Perspectives Using Q Methodology. The R Journal, 6(2):163-173.

library(tidyverse)
?qmethod

data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
summary(results)
results
plot(results)

lipset

### 결과 출력 방법 ###
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
print(results, length = 5, digits = 1)



### 그래프 그리는 방법 ###
## S3 method for class 'QmethodRes'
plot(x, xlab = 'z-scores', ylab = 'statements', 
     pchlist = NULL, colours = NULL, 
     fnames = NULL, legend = TRUE,
     dist = TRUE, pchlist.fill = NULL,
     leg.pos="bottomright", xlim= NULL, 
     sort.items=T, factors = NULL,
     ...)

data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
title <- "Q method z-scores, lipset dataset"
subtitle <- paste0("Three factors, PCA, varimax. Printed on ",
                   Sys.Date())
plot(results, main = title, sub = subtitle)

# Order the items in a different way
plot(results, main = title, sub = subtitle, 
     sort.items = c(rev(1:nrow(results$zsc))))
