\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{'eau'
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    stopifnot(is.data.frame(graph), colnames(graph) \%in\% c("v1", 
        "v2", "w"), is.numeric(init_node) && length(init_node) == 
        1, init_node \%in\% graph$v1 || init_node \%in\% graph$v2)
    nodes <- unique(graph$v1)
    dist <- rep(Inf, length(nodes))
    prev <- rep(NA, length(nodes))
    dist[init_node] <- 0
    Q <- nodes
    while (length(Q) > 0) {
        u <- Q[which.min(dist[Q])]
        Q <- setdiff(Q, u)
        neighbors <- c(graph$v2[graph$v1 == u])
        for (v in neighbors) {
            alt <- dist[u] + graph$w[which((graph$v1 == u & graph$v2 == 
                v))]
            if (alt < dist[v]) {
                dist[v] <- alt
                prev[v] <- u
            }
        }
    }
    return(dist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
