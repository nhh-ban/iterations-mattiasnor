library(glue)

vol_qry <- function(id, from, to) {
  query_string <- glue(
    '{
      trafficData(trafficRegistrationPointId: "{id}") {
        volume {
          byHour(from: "{from}", to: "{to}") {
            edges {
              node {
                from
                to
                total {
                  volumeNumbers {
                    volume
                  }
                }
              }
            }
          }
        }
      }
    }',
    .open = '"{', 
    .close = '}"'
  )
  return(query_string)
}
