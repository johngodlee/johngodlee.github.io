# Analysing survey likert data

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import data and clean data ----
survey <- read.csv("example.csv", na.strings=c("","NA"), stringsAsFactors = FALSE)

# Remove NA's
survey[is.na(survey)] <- ''

# Remove first row
survey_header <- survey %>%
	slice(2:n())

# Concatenate question columns
survey_employee <- survey_header %>%
	select(1:4) 

survey_again <- survey_header %>%
	select(5:9)

survey_always <- survey_header %>%
	select(10:14)

survey_line <- survey_header %>%
	select(15:19)

survey_think <- survey_header %>%
	select(20:24)

# Concatenate columns 
what_type_of_employee_are_you <- unite(survey_employee, what_type_of_employee_are_you, 1:4, sep='', remove=F)[1]
i_would_work_for_comx_again <- unite(survey_again, i_would_work_for_comx_again, 1:5, sep='', remove=F)[1]
i_am_always_busy_at_comx <- unite(survey_always, i_am_always_busy_at_comx, 1:5, sep='', remove=F)[1]
my_line_manager_values_my_contributions <- unite(survey_line, my_line_manager_values_my_contributions, 1:5, sep='', remove=F)[1]
i_think_directors_are_paid_the_right_amount <- unite(survey_think, i_think_directors_are_paid_the_right_amount, 1:5, sep='', remove=F)[1]

# Combine into data frame 
survey_cond <- data.frame(what_type_of_employee_are_you, i_would_work_for_comx_again,
													i_am_always_busy_at_comx, my_line_manager_values_my_contributions,
													i_think_directors_are_paid_the_right_amount)

# Give numerical values to data ----
survey_cond_num <- survey_cond %>%
	mutate(i_would_work_for_comx_again = recode(i_would_work_for_comx_again, 
																							"Strongly disagree" = -2, 
																							"Disagree" = -1, 
																							"Neither agree nor disagree" = 0, 
																							"Agree" = 1, 
																							"Strongly agree" = 2),
				 i_am_always_busy_at_comx = recode(i_am_always_busy_at_comx, 
				 																	"Strongly disagree" = -2, 
				 																	"Disagree" = -1, 
				 																	"Neither agree nor disagree" = 0, 
				 																	"Agree" = 1, 
				 																	"Strongly agree" = 2),
				 my_line_manager_values_my_contributions = recode(my_line_manager_values_my_contributions, 
				 																								 "Strongly disagree" = -2, 
				 																								 "Disagree" = -1, 
				 																								 "Neither agree nor disagree" = 0, 
				 																								 "Agree" = 1, 
				 																								 "Strongly agree" = 2),
				 i_think_directors_are_paid_the_right_amount = recode(i_think_directors_are_paid_the_right_amount, 
				 																										 "Strongly disagree" = -2, 
				 																										 "Disagree" = -1, 
				 																										 "Neither agree nor disagree" = 0, 
				 																										 "Agree" = 1, 
				 																										 "Strongly agree" = 2))

# Create pivot tables ----
heirarchy <- c("Director", "Consultant", "HR", "Admin")
col_names <- names(select(survey_cond, 2:5))

summ_all <- survey_cond %>%
	select(what_type_of_employee_are_you, col_names) %>%
	gather(key, value, -what_type_of_employee_are_you) %>%
	split(.$key) %>%
	lapply(function(x){x %>% group_by(what_type_of_employee_are_you, value) %>%
			tally() %>%
			spread(value, n, fill = 0) %>%
			ungroup() %>%
			mutate(what_type_of_employee_are_you = factor(what_type_of_employee_are_you, levels = heirarchy)) %>%
			arrange(what_type_of_employee_are_you) %>%
			select(what_type_of_employee_are_you, 
						 `Strongly disagree`, 
						 Disagree, 
						 `Neither agree nor disagree`,
						 Agree,
						 `Strongly agree`)
	}) 

# Write to csv
for (i in seq_along(summ_all)) {
	write.csv(summ_all[[i]], paste("pivot_tables/",names(summ_all[i]), ".csv", sep = ""))
}

# Example of a pivot table made into a bar chart
pivot <- read.csv("pivot_tables/i_am_always_busy_at_comx.csv")

resp_order <- c("Strongly.disagree", "Disagree", "Neither.agree.nor.disagree", "Agree", "Strongly.agree")

pivot_gather <- pivot %>%
	select(2:7) %>%
	gather(Response, Score, Strongly.disagree:Strongly.agree) %>%
	mutate(Response = factor(Response, levels = resp_order)) %>%
	mutate(Role = factor(what_type_of_employee_are_you, levels = heirarchy))

ggplot(pivot_gather, aes(x = Role, y = Score, fill = Response)) +
	geom_bar(stat = "identity", position = "dodge") + 
	scale_fill_brewer(palette="Blues") + 
	theme(legend.title = element_blank()) + 
	ggtitle("I am always busy at comX")

# questions ranked by total score ----
survey_total_q <- survey_cond_num %>% 
select(1:5) %>%
	summarise_all(funs(mean(., na.rm = TRUE))) %>%
	gather("question","mean_score") %>%
	na.omit(TRUE) %>%
	arrange(desc(mean_score))

ggplot(survey_total_q, aes(x = reorder(question,-mean_score), y = mean_score, fill = mean_score)) + 
	geom_bar(stat = "identity") + 
	coord_flip() + 
	theme(axis.title.y = element_blank()) +
	scale_fill_continuous(low = "#E33235", high = "#2183EB") + 
	theme(legend.position="none") +
	ylab("Mean Likert Score")

# Stacks by employee type ----
survey_total_job <- survey_cond_num %>% 
	select(1:5) %>%
	group_by(what_type_of_employee_are_you) %>%
	na.omit(TRUE) %>%
	summarise_all(funs(sum)) %>%
	gather("question","total_score") %>%
	mutate(what_type_of_employee_are_you = strrep(c("Director", "Consultant", "HR", "Admin"), times = 1)) %>%
	group_by(question, what_type_of_employee_are_you) %>%
	filter(question != "Role") %>%
	arrange(desc(total_score))

ggplot(survey_total_job[order(survey_total_job$total_score, decreasing = T),],
			 aes(x = reorder(question,total_score), y = total_score, fill = what_type_of_employee_are_you)) + 
	geom_bar(stat = "identity") + 
	coord_flip() + theme(axis.title.y = element_blank()) + 
	scale_fill_brewer(limits = heirarchy, palette = "Dark2") +
	scale_x_discrete(limits = as.vector(survey_total_q$question)) +
	theme(axis.text.x = element_blank(),
				axis.ticks.x = element_blank()) + 
	guides(fill=guide_legend(title="Role")) + 
	xlab("Total Likert Score")

# Total question score by type of employee ----
role_total <- survey_cond_num %>%
	select(1:5) %>%
	replace(is.na(.), 0) %>%
	group_by(what_type_of_employee_are_you) %>%
	summarise_all(funs(sum)) %>%
	mutate(sum = rowSums(.[2:length(.)]))

ggplot(role_total, aes(x = what_type_of_employee_are_you, y = sum, fill = what_type_of_employee_are_you)) + 
	geom_bar(stat = "identity") +
	ylab("Total Likert Score") + 
	scale_x_discrete(limits = heirarchy) + 
	guides(fill=guide_legend(title="Role")) + 
	scale_fill_brewer(palette = "Dark2")
