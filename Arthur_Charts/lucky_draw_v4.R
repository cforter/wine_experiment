data <- read.csv("Data_Lucky_Draw.csv")

#lucky_draw function (totally random with names)
lucky_draw <- function(dataset){
    winners <- as.data.frame(matrix(,nrow=3,ncol=2))
    colnames(winners) <- c("wine_type","winner_name")
    wine_types <- c('cheap','medium','expensive')
    for (i in 1:length(wine_types)){
        data.wine_type <- dataset[dataset$wine_pref==wine_types[i],]
        raffle_tickets <- vector()
        for (j in 1:nrow(data.wine_type)){
            name<-as.character(data.wine_type[j,'name'])
            raffle_tickets<-c(raffle_tickets,name)
        }
        
        print(raffle_tickets)    #check if the correct raffle tickets get created
        winners[i,] <- c(wine_types[i],sample(raffle_tickets,1))
    }
    return(winners)
}
lucky_draw(data)



# #lucky_draw function (based on score)
# lucky_draw <- function(dataset){
#     winners <- as.data.frame(matrix(,nrow=3,ncol=2))
#     colnames(winners) <- c("wine_type","winner_id")
#     wine_types <- c('cheap','medium','expensive')
#     for (i in 1:length(wine_types)){
#         data.wine_type <- dataset[dataset$wine_pref==wine_types[i],]
#         raffle_tickets <- vector()
#         for (j in 1:nrow(data.wine_type)){
#             raffle_tickets<-c(raffle_tickets,rep(data.wine_type[j,'id'],data.wine_type$quiz_score[j]))
#         }
# #         print(raffle_tickets)    #check if the correct raffle tickets get created
#         winners[i,] <- c(wine_types[i],sample(raffle_tickets,1))
#     }
#     return(winners)
# }
# lucky_draw(data)


