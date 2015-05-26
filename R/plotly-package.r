#' plot.ly is a browser-based data visualization tool that creates interactive and publication 
#' quality figures. This API allows R users to generate plot.ly graphs from their desktop 
#' R environment.
#' 
#' An example of an interactive graph made from the R API: https://plot.ly/~chris/407/
#' 
#' \itemize{
#'  \item Package: plotly
#'  \item Type: Package
#'  \item Version: 0.6.2
#'  \item Date: 2014-03-07
#'  \item License: MIT
#' }
#' 
#' @section Authentication:
#' 
#' There are a few different options. First, you can pass in your username and key 
#' to the \code{plotly} function as parameters, like \code{plotly(<yourusername>, 
#' <yourkey>)}. Second, you can set your username and key as options temporarily
#' within each R session by executing \code{options(plotlyUsername='<yourusername>')}
#' and \code{options(plotlyKey='<yourkey>')}. Third, you set your username and key
#' permanently (until removed) in your .Rprofile file. Put entries in for each of 
#' username and password: \code{options(plotlyUsername='<yourusername>')} and 
#' \code{options(plotlyKey='<yourkey>')}. 
#' 
#' @author Chris Parmer chris@@plot.ly
#' @references Documentation and examples at https://plot.ly/API
#' @name plotly-package
#' @docType package
#' @title A R API for plot.ly
#' @keywords package
NULL 
