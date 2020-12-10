fit.linear <- (final_vote~approved+comfort_score+service_score+food_score+enjoyment_score+station_score
               +internet_connection_score+expendiency_score)

fit.poly2.complete <- (final_vote~approved+poly(comfort_score,2)+poly(service_score,2)
                       +poly(food_score,2)+poly(enjoyment_score,2)+poly(station_score,2)
                       +poly(internet_connection_score,2)+poly(expendiency_score,2))

fit.poly2 <- (final_vote~approved+station_score+comfort_score+internet_connection_score
              +expendiency_score+I(enjoyment_score^2)+I(comfort_score^2)+I(food_score^2)+I(station_score^2)
              +I(service_score^2)+I(expendiency_score^2))

fit.poly3 <- (final_vote~approved+poly(comfort_score,3)+poly(service_score,3)
              +poly(food_score,3)+poly(enjoyment_score,3)+poly(station_score,3)
              +poly(internet_connection_score,3)+poly(expendiency_score,3))

fit.poly4 <- (final_vote~approved+poly(comfort_score,4)+poly(service_score,4)
              +poly(food_score,4)+poly(enjoyment_score,4)+poly(station_score,4)
              +poly(internet_connection_score,4)+poly(expendiency_score,4))


fit.log <- (final_vote~approved+log(comfort_score)+log(service_score)
            +log(food_score)+log(enjoyment_score)+log(station_score)+log(expendiency_score))