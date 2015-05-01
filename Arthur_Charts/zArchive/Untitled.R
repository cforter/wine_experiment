data <- read.csv("Data_Collection_RealDeal - R_data.csv")

#lucky_draw function
lucky_draw <- function(dataset){
    winners <- as.data.frame(matrix(,nrow=3,ncol=2))
    colnames(winners) <- c("wine_type","winner_id")
    wine_types <- c('cheap','medium','expensive')
    for (i in 1:length(wine_types)){
        data.wine_type <- dataset[dataset$wine_pref==wine_types[i],]
        raffle_tickets <- vector()
        for (j in 1:nrow(data.wine_type)){
            raffle_tickets<-c(lucky_draw,rep(data.wine_type[j,'id'],data.wine_type$quiz_score[j]))
        }
        print(raffle_tickets)
        winners[i,] <- c(wine_types[i],sample(raffle_tickets,1))
    }
    return(winners)
}
lucky_draw(data)
