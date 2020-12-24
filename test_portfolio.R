source("src/packages.R")   # load packages for analysis
source("src/functions.R")  # load helper functions

# example showing the functions at work - this will save an object in the global env
monthly_returns("SBUX", 2015, glb_env = T)
daily_returns("SBUX", 2015, glb_env = T)
yearly_returns("SBUX", 2015, glb_env = T)

# remove examples
base::rm(SBUX, SBUX_daily, SBUX_yearly)

# create a vector of all the stock tickers we want to analyze

# for the code at the bottom of the script to work, always have your benchmark last
# and only use one benchmark
tickers_vec <- c("SBUX", "CCL", "AAPL", "DIS", "LULU", "NKE", "TGT", "FB", "T", "LUV", "SPY")

# APPLY FUNC METHOD ----------------------------------------------------------------------

# use apply function to create list of data. This is cleaner than calling the function multiple times
stock_lst <- base::lapply(tickers_vec,
                          monthly_returns,
                          base_year = 2015,
                          glb_env = FALSE)

names(stock_lst) <- tickers_vec

# LOOP METHOD ---------------------------------------------------------------------------

# we can also do this with a for loop
lst <- list()

for (i in tickers_vec) {
  x <- monthly_returns(i, 2017)
  lst[[i]] <- x
}

# Merge all the data into and XTS object and rename columns
returns <- base::do.call(merge, stock_lst)
base::colnames(returns) <- tickers_vec

# Produce interactive chart of stock returns
dygraphs::dygraph(returns, main = "Basics vs. S&P 500") %>%
  dygraphs::dyAxis("y", label = "Return", valueRange = c(-1, 0.75)) %>%
  dygraphs::dyRangeSelector(dateWindow = c("2015-01-01", "2020-12-31")) %>%
  dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))

# Print last 5 rows of the data, rounded to 4 decimal places
round(tail(returns, n = 5), 4)

# get the correlation matrix:
corrplot::corrplot(cor(returns), method = "number")

# Assign weights evenly for all stocks we want in the portfolio we are creating
length <- length(tickers_vec) - 1 # subtract 1 becasue SPY is our benchmark
weight <- 1/length # calculate weight
wts <- rep(weight, length) # replicate weight as many times as stocks in portfolio

# Construct a portfolio using our returns object and weights
portfolio_returns <- Return.portfolio(R = returns[,1:length], weights = wts, wealth.index = F)

# Then isolate our S&P 500 data
benchmark_returns <- Return.portfolio(R = returns[,11], wealth.index = F)

# Merge the benchmark and portfolio together
comp <- xts::merge.xts(portfolio_returns, benchmark_returns)
colnames(comp) <- c("Portfolio", "Benchmark")

# Build an interactive graph to compare performance
dygraphs::dygraph(comp, main = "Portfolio Performance vs. Benchmark") %>%
  dygraphs::dyAxis("y", label = "Amount ($)")

# cumualtive returns
PerformanceAnalytics::Return.cumulative(comp$Portfolio, geometric = TRUE)
PerformanceAnalytics::Return.cumulative(comp$Benchmark, geometric = TRUE)

# test an investment amount to see potential growth
investment <- 10000
myReturn_pct <- PerformanceAnalytics::Return.cumulative(comp$Portfolio, geometric = TRUE)[1]
myReturn <- myReturn_pct * investment
totReturn <- myReturn + investment
print(paste0("10,000 invested in the Portfolio at the start of the sample period would have GROWN TO $", 
             formatC(totReturn, format="f", big.mark=",", digits=1) , " by the end of the sample period"))

# plot cumulative returns of Portfolio VS Benchmark
PerformanceAnalytics::chart.CumReturns(comp, wealth.index = FALSE, 
                                       geometric = TRUE, legend.loc = 'bottomright')


