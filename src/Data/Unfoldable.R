list(
  unfoldrArrayImpl = function(isNothing) {
    function(fromJust) {
      function(fst) {
        function(snd) {
          function(f) {
            function(b) {
              result <- list()
              value <- b
              while (T) {
                maybe <- f(value)
                if (isNothing(maybe)) {
                  return(result)
                }
                tuple <- fromJust(maybe)
                result <- c(result, fst(tuple))
                value <- snd(tuple)
              }
            }
          }
        }
      }
    }
  }
)
