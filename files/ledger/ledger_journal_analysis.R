# Ledger journal analysis

# Packages
library(dplyr)
library(ggplot2)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Functions
ggplot_theme <- function(){
	theme_bw() +
		theme(axis.text = element_text(size = 12),
					axis.title = element_text(size = 12),
					axis.line.x = element_line(color="black"), 
					axis.line.y = element_line(color="black"),
					panel.border = element_blank(),
					panel.grid.major.x = element_blank(),
					panel.grid.minor.x = element_blank(),
					panel.grid.minor.y = element_blank(),
					panel.grid.major.y = element_blank(),
					plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
					legend.text = element_text(size=12),
					legend.key = element_blank())
	}

stacked_bar_theme <- function(){
	ggplot_theme() +
		theme(axis.text.x = element_blank(),
					axis.text.y = element_text(size = 12),
					axis.title.x = element_blank(),
					axis.title.y = element_text(size = 12),
					axis.line.x = element_line(color="black"), 
					axis.line.y = element_line(color="black"),
					axis.ticks.x = element_blank())
	}

bar_no_legend_theme <- function(){
	ggplot_theme() + 
		theme(legend.position = "none")
	}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	require(grid)
	
	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)
	
	numPlots = length(plots)
	
	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
										 ncol = cols, nrow = ceiling(numPlots/cols))
	}
	
	if (numPlots==1) {
		print(plots[[1]])
		
	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
		
		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
			
			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
																			layout.pos.col = matchidx$col))
		}
	}
}

# Import and fix ledger csv
journal_names <- c("Date", "NA_1", "Description", "Source", "Currency", "Amount", "Pending_Cleared", "NA_2")
ledger <- read.csv("example.csv", col.names = journal_names)

# Get basic info
head(ledger)
ledger

# Fix date column
ledger$good_date <- as.Date(ledger$Date, format = "%Y/%m/%d")
class(ledger$good_date)

# Sort by good_date - helps geom_text work better
ledger_sort <- ledger[order(ledger$good_date),]

# Add cumulative column for each source
ledger_cumsum <- ledger_sort %>%
	group_by(Source) %>%
	mutate(Cumulative = cumsum(Amount))

# Create df only with Assets
Assets <- ledger_cumsum %>%
	filter(grepl("Assets", Source))

# Line plot of Assets over time
p1 <- ggplot(Assets, aes(x = good_date, y = Cumulative, group = Source)) + 
	geom_hline(aes(yintercept = 0), colour = "red") +
	geom_line(aes(colour = Source), size = 1.2) + 
	geom_point(aes(colour = Source), size = 2) +
	scale_x_date(date_breaks = "1 month", date_labels = "%W/%y") + 
	ggplot_theme() + 
	xlab("Date WW-YY") + 
	ylab("Balance ($)")

# Create df only with student account 
Assets_bank_student <- ledger_cumsum %>%
	filter(Source == "Assets:Checking")

# Line plot of student account over time with description of expenditure
p2 <- ggplot(Assets_bank_student, aes(x = good_date, y = Cumulative, group = Source, label = Description)) + 
	geom_line() + 
	geom_text() + 
	scale_x_date(date_breaks = "2 days", date_labels = "%W/%y") + 
	ggplot_theme() + 
	xlab("Date WW-YY") + 
	ylab("Balance ($)")

# Create df of Expenses summary
Expenses_sum <- ledger_cumsum %>%
	filter(grepl("Expenses", Source)) %>%
	group_by(Source) %>%
	summarise(Amount = sum(Amount)) %>%
	mutate(Percentage = Amount / sum(Amount) * 100) %>%
	mutate(Source = factor(Source, levels = Source[order(Amount, decreasing = TRUE)]))  # Create ordered factor for x axis

# Bar plot of Expenses
p3 <- ggplot(Expenses_sum, aes(x = Source, y = Amount)) + 
	geom_bar(stat = "identity", aes(fill = Source)) + 
	theme(legend.position = "none") +
	ylab("Amount ($)") + 
	bar_no_legend_theme()

# Stacked Bar plot of Expenses percentage
p4 <- ggplot(Expenses_sum, aes(x = NA, y = Percentage, fill = Source)) + 
	geom_bar(stat = "identity") + 
	geom_text(aes(label = paste(round(Percentage, digits = 2), "% - ", Source, sep="")), 
						position=position_stack(vjust=0.5)) + 
	stacked_bar_theme()

# Last 30 days income/expense summary
ledger_30d_summ <- ledger_cumsum %>%
	filter(good_date > as.Date(Sys.Date(), format = "%Y-%m-%d") - 10000) %>%
	filter(grepl("Assets", Source)) %>%
	mutate(expense_income = if_else(Amount > 0, "Income", "Expense")) %>%
	group_by(expense_income) %>%
	summarise(Total = sum(Amount)) %>%
	mutate(Total = abs(Total))

	ledger_30d_summ
# Bar plot income vs Expenses for last 30 days.
expense_income_palette <- c("#D43131", "#1CB5DB")  # Colour palette

p5 <- ggplot(ledger_30d_summ, aes(x = expense_income, y = Total), environment = environment()) + 
	geom_bar(stat = "identity", fill = expense_income_palette) + 
	geom_errorbar(aes(x = ifelse(ledger_30d_summ$Total[1] > ledger_30d_summ$Total[2], "Income", "Expense"), 
									ymax = max(ledger_30d_summ$Total),
									ymin = min(ledger_30d_summ$Total))) +
	geom_text(aes(x = ifelse(ledger_30d_summ$Total[1] > ledger_30d_summ$Total[2], "Income", "Expense"),
								y = min(ledger_30d_summ$Total) + 0.5*(max(ledger_30d_summ$Total) - min(ledger_30d_summ$Total)),
								label = ifelse(ledger_30d_summ$Total[1] > ledger_30d_summ$Total[2],
															 paste("$ -", max(ledger_30d_summ$Total) - min(ledger_30d_summ$Total), sep = ""),
															 paste("$ ", max(ledger_30d_summ$Total) - min(ledger_30d_summ$Total), sep = "")),
								hjust = -0.5)) + 
	xlab("Expense/Income") + 
	ylab("Amount ($)") + 
	ggplot_theme()
