# filepath where the supportFunctions.R is placed
source('./R/supportFunctions.R')
library('getPass')

username <- getPass('Enter a JIRA username:')
password <- getPass('Enter a JIRA password:')
filterId <- getPass('Enter the JIRA filter id:')

options("jira_url" = "https://jira.maaslab.uspa.ibm.com")
options("jira_user" = username)
options("jira_password" = password)
options("jira_filter" = filterId)

# Clear memory, so that these are not listed under global environment
remove(username)
remove(password)

# declarations
isDone <- F
count <- 0
countPerPage <- 50
#maxPages <- 500
maxPages <- 5
issues <- c()

while (!isDone) {
  page <- (count / countPerPage) + 1

  print(paste0('Getting issues on page ', page))

  currentIssues <- get_issues(start=count)
  if (length(currentIssues) > 0 && page <= maxPages) {
    issues <- c(issues, currentIssues)
    sapply(issues, function(issue) {
      issues <- rbind(issues, issue)
    })

    print(paste0('Found ', length(currentIssues), ' issues (', length(issues), ' total)'))

    count <- count + length(currentIssues)
  }
  else {
    isDone <- T
  }
}

print(paste0('Found ', length(issues), ' issues.'))

# Format the results into a filterResults data-set.
# If your projects have different  fields, you may have to henadle them here
filterResults <- as.data.frame(t(sapply(issues, function(issue) {
  list(Key=unlist(issue$key), issuetype=unlist(issue$fields$issuetype$name), priority=unlist(issue$fields$customfield_10751$value), created=unlist(issue$fields$created), status=unlist(issue$fields$status$name), squad=unlist(ifelse(is.null(issue$fields$customfield_15870$value), NA, issue$fields$customfield_15870$value)), reporter=unlist(ifelse(is.null(issue$fields$reporter$name), NA, issue$fields$reporter$name)), assignee=unlist(ifelse(is.null(issue$fields$assignee$name), NA, issue$fields$assignee$name)), summary=unlist(issue$fields$summary), description=ifelse(is.null(issue$fields$description), NA, unlist(issue$fields$description)))
})))

# Unlist the columns.
sapply(names(filterResults), function(colname) {
  filterResults[colname] <<- unlist(filterResults[colname])
})

# Set column types.
filterResults <- as.data.frame(filterResults)
filterResults$Key <- as.factor(filterResults$Key)
filterResults$issuetype <- as.factor(filterResults$issuetype)
filterResults$priority <- as.factor(filterResults$priority)
filterResults$created <- as.POSIXct(unlist(filterResults$created), format='%Y-%m-%dT%H:%M:%OS')
filterResults$status <- as.factor(filterResults$status)
filterResults$squad <- as.factor(filterResults$squad)
filterResults$assignee <- as.factor(filterResults$assignee)
filterResults$reporter <- as.factor(filterResults$reporter)

# Sort by created date descending.
filterResults <- filterResults[order(filterResults$created, decreasing=T),]

# Output to csv.

write.csv(file='./R/issues.csv', filterResults)
