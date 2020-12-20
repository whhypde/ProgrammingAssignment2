makeVector <- function(x = numeric()) {
        m <- NULL
        # set is a function that returns
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get is a function that returns the input x
        get <- function() x
        # setmean is a function that assigns mean to m
        setmean <- function(mean) m <<- mean
        # getmean is a function that gets the value m
        getmean <- function() m
        # the function makeVector returns the four functions which are just
        # defined
        list(
                set = set, get = get,
                setmean = setmean,
                getmean = getmean
        )
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

plusx <- function(x) {
        a <<- 1 + x
        # with the operator <<- the value of a computed here is sent outside
        b <- 2 + x
        # with the original assignment operator <- the value of b is not valid
        # outside but only inside this function
        x <- x * 2
        # the value of x computed here is also only valid inside the function
        c(b, x)
}

list_data <- list("Red", "Green", c(21, 32, 11), TRUE, 51.23, 119.1)
list_data2 <- c("Red", "Green", c(21, 32, 11), TRUE, 51.23, 119.1)
print(list_data)
class(list_data[3])