library(httr)

#' @name search_url
#' @title Get jira search url
#' @param jira_url base url to jira. Defaults to 'jira/'
#' @return string
#' search_url()
search_url <- function(jira_url = getOption("jira_url")){

  if(is.null(jira_url))
    return(NULL)

  search_url <- file.path(jira_url, "rest/api/latest/search?")

  return(search_url)

}
#' @name jira_get
#' @title Get jira authenticated for results
#' @return string
jira_get <- function(url = url, user = user, password = password, verbose = verbose){

  res <- GET(url = url,
             authenticate(user = user, password = password, "basic"),
             add_headers("Content-Type" = "application/json"),
             verbose(data_out = verbose, data_in = verbose, info = verbose)
  )

  return(res)
}
#' @name get_issues
#' @title Get jira results for the specified filter id
#' @return list
get_issues <- function(user = NULL
                       , jira_filter = getOption("jira_filter")
                       , jira_url = getOption("jira_url")
                       , jira_user = getOption("jira_user")
                       , jira_password = getOption("jira_password")
                       , verbose = getOption("jira_verbose"), start=NULL){
  print(paste0("jira_url: ", jira_url))
  print(paste0("jira_user: ", jira_user))
  print(paste0("jira_password: ", jira_password))
  print(paste0("jira_verbose: ", verbose))
  print(paste0("jira_filter: ", jira_filter))
  if(is.null(jira_url))
    stop('jira_url is NULL. See getOption("jira_url")')

  if(is.null(jira_user))
    stop("jira_user is NULL")

  if(is.null(jira_password))
    stop("jira_password is NULL")

  url <- search_url(jira_url = jira_url)
  print(paste0("Search url: ", url))
  if(!is.null(jira_filter)){
    url <- paste0(url, sprintf('jql=filter="%s"', jira_filter))
  }
  print(paste0("url: ", url))
  if(!is.null(start))
    url <- paste0(url, sprintf('&startAt=%s', start))

  res <- jira_get(url = url, user = jira_user, password = jira_password, verbose = verbose)
  res <- content(res, as = "parsed")
  res <- res$issues
  return(res)
}
