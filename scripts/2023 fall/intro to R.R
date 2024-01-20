# EXAMPLE 1
# install a package
install.packages('devtools')
devtools::install_github("brooke-watson/BRRR")
# load a library
library(BRRR)
# run a function from the BRRR package
skrrrahh(0)
skrrrahh_list()

# EXAMPLE 2
install.packages('gtrendsR')
library(gtrendsR)
# run a query
trends <- gtrends("cats", geo = "CR", time = "all")
plot(trends)
# run a 2nd query
trends2 <- gtrends(c("coffee", "Harry Styles"), geo = "IE", time = "today 1-m")
plot(trends2)
