# Analyse ledger CSV file
# John Godlee (johngodlee@gmail.com)
# 2022-03-12

# Packages
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define journal file
infile <- "~/.ledger.journal"

# Create temporary csv
csvfile <- tempfile(fileext = ".csv")
on.exit(unlink(csvfile))

# Define string to convert ledger to csv
csv_fmt <- '"%(quoted(date)),%(quoted(display_account)),%(quoted(quantity(scrub(display_amount))))\n"'

# Convert journal to csv
system(paste(c(
      "ledger", "csv", 
      "-f", infile, 
      "--csv-format", csv_fmt,
      "-o", csvfile), collapse = " "))

# Import journal csv
col_names <- c("date", "source", "amount")
jrnl <- read.csv(csvfile, col.name = col_names)

# Clean journal
jrnl_clean <- jrnl %>% 
  mutate(date_fix = as.Date(date, format = "%Y/%m/%d")) %>%
  arrange(date) %>%
	group_by(source) %>%
	mutate(cum = cumsum(amount))


# Dataframe with assets only
assets <- jrnl_clean %>% 
  filter(grepl("^assets", source)) %>%
  mutate(source = gsub("^assets:", "", source)) 

# Line plot of assets over time
ggplot() + 
	geom_hline(aes(yintercept = 0), colour = "red", linetype = 2) +
	geom_line(data = assets, 
	  aes(x = date_fix, y = cum, colour = source)) + 
	scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
	theme_bw() + 
	labs(x = "Date", y = "Balance (£)")

# Create dataframe of expenses summary
expenses_summ <- jrnl_clean %>%
	filter(grepl("^expenses", source)) %>%
	group_by(source) %>%
	summarise(amount = sum(amount)) %>%
  mutate(source = gsub("^expenses:", "", source)) %>% 
	mutate(source = factor(source, 
	    levels = source[order(amount, decreasing = TRUE)]))  

# Bar plot of expenses by group
ggplot(expenses_summ, aes(x = source, y = amount)) + 
	geom_bar(stat = "identity", aes(fill = source)) + 
	theme_bw() + 
	theme(legend.position = "none") + 
  theme(axis.text = element_text(size=12, angle=45, vjust=1, hjust=1))

# Last 30 days income/expense summary
jrnl_30d <- assets %>%
	filter(date_fix > as.Date(Sys.Date(), format = "%Y-%m-%d")-30) %>%
	mutate(expense_income = if_else(amount > 0, "income", "expense")) %>%
	group_by(expense_income) %>%
	summarise(total = sum(amount)) %>%
	mutate(total = abs(total))

# Bar plot income vs expenses for last 30 days.
expense_income_pal <- c("#D43131", "#1CB5DB")  # Colour palette

ggplot(jrnl_30d, aes(x = expense_income, y = total)) +
  geom_bar(stat = "identity", fill = expense_income_pal) + 
  geom_errorbar(aes(x = ifelse(total[1] > total[2], "income", "expense"), 
    ymax = max(total), ymin = min(total))) +
  geom_text(aes(
      x = ifelse(total[1] > total[2], "income", "expense"), 
      y = min(total) + 0.5*(max(total) - min(total)),
      label = ifelse(total[1] > total[2],
        paste("£ -", max(total) - min(total), sep = ""),
        paste("£ ", max(total) - min(total), sep = "")),
      hjust = -0.5)) + 
  xlab("") + 
  ylab("Amount (£)") + 
  theme_bw()

# Total equity over time
equity <- assets %>% 
  group_by(date_fix) %>% 
  summarise(total = sum(amount)) %>% 
  mutate(cum = cumsum(total))

ggplot() + 
  geom_line(data = equity, aes(x = date_fix, y = cum)) + 
	scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
	theme_bw() +
	labs(x = "Date", y = "Balance (£)")
