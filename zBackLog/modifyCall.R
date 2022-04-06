
#' @importFrom utils modifyList
modifyCall <- function(cl, additionalArgs){
	as.call(modifyList(as.list(cl), additionalArgs))
}

if (F){
	modifyCall(quote(f(x= 1, y = 2)), list(z = 3))
	modifyCall(quote(f(x= 1, y = 2)), list(x = 3))
	modifyCall(quote(f(x= 1, y = 2)), list(z = 4, x = 3, w = 5))
	modifyCall(quote(f(x= 1, y = 2)), list(z = 4, x = 3))

}
	