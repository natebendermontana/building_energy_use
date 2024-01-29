install.packages("cronR")
library(cronR)
library(here)
# list the contents of a crontab
cron_ls()
# list the full path of where the rscript is located
path = here("bldg_energy_dataprep_ongoing.R")
# Create a command to execute an R-script
cmd = cron_rscript(path)
# add the command and specify the days/times to start
cron_add(command = cmd, frequency = 'daily', at="07:30", days_of_week = c(1:7),
         id = 'daily_newdata', description = 'Energy Insights app - daily new data')

# remove it by 'id' if needed
# cron_rm(id = "daily_newdata")

# cron schedule for days of the week:
# 0 - Sunday
# 1 - Monday
# 2 - Tuesday
# 3 - Wednesday
# 4 - Thursday
# 5 - Friday
# 6 - Saturday
# 7 - Sunday