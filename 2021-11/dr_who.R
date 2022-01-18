library(tidyverse)
library(datardis)

# datardis data sets
data(directors)
data(episodes)
data(writers)

glimpse(directors)
glimpse(episodes)
glimpse(writers)

x <- writers |> 
  count(writer) |> 
  arrange(desc(n))

split_summary <- function(x, split_at = 1, 
                          name_col = NULL, val_col = NULL,
                          summary_name = "other") {
  
  if (is.null(name_col)) name_col = 1
  if (is.null(val_col)) val_col = 2
  if (!is.numeric(x[[val_col]])) error("val_col must be numeric")
  if (split_at > nrow(x)) {
    warning("split_at position is beyond vector length; setting to 1")
    split_at = 1
  }
  
  y_name_col <- x[[name_col]][1:split_at]
  y_val_head <- x[[val_col]][1:split_at]
  y_val_tail <- x[[val_col]][(split_at+1):length(x[[val_col]])] 
  y_val_tail_sum <- if (split_at == nrow(x)) {
    0
  } else {
    y_val_tail |> sum(na.rm = TRUE)
  }
  
  headdf <- data.frame(y_name_col, y_val_col = y_val_head)
  taildf <- data.frame(y_name_col = summary_name, 
                       y_val_col = y_val_tail_sum)
  outdf <- rbind(headdf, taildf)
  names(outdf) <- names(x[1:2])
  outdf
}

data.frame(name = c("Alice", "Bob", "Cindy", "Dave"),
           papers = c(8, 6, 11, 7)) |> 
  arrange(desc(papers)) |> 
  split_summary(5, summary_name = "and the rest")

