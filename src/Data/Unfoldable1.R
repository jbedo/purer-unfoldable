list(
  unfoldr1ArrayImpl = function(isNothing) {
    function(fromJust) {
      function(fst) {
        function(snd) {
          function(f) {
            function(b) {
              result <- list()
              value <- b
              while (T) {
                tuple <- f(value)
                result <- c(result, fst(tuple))
                maybe <- snd(tuple)
                if (isNothing(maybe)) {
                  return(result)
                }
                value <- fromJust(maybe)
              }
            }
          }
        }
      }
    }
  }
)
