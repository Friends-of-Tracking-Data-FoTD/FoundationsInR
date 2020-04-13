library(rjson)
#library(jsonlite) [you can try jsonlite::fromJSON(x,flatten=TRUE) to see what that does when reading in the JSON file]
library(data.table)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

####Obtain Competitions####

#Read File from JSON into a list 
competitions <- fromJSON(file="C:\\Users\\sgopaladesikan\\Desktop\\open-data-master\\data\\competitions.json")


#Convert List into a DataFrame
competitions.df <- data.frame(do.call(rbind,competitions),stringsAsFactors = FALSE)


####Obtain Matches####
match.files <- list.files(path="C:\\Users\\sgopaladesikan\\Desktop\\open-data-master\\data\\matches",
                          full.names = TRUE,recursive = TRUE)

matches.list <- list()
for(i in 1:length(match.files)){
  match.temp <- fromJSON(file=match.files[i]) ##Loop through each file which contains all the matches for a given competition and season and obtain the necessary match information
  
  matches <- lapply(match.temp, function(x) data.frame(t(unlist(x)),stringsAsFactors = FALSE))
  matches.df <- rbindlist(matches,fill=TRUE) #we use rbindlist instead of do.call(rbind,) because of column mismatch
  matches.list[[i]] <- matches.df #this assigns matches.df to the matches.list list that we initialized 
  
}

all.matches.df <- data.frame(rbindlist(matches.list,fill=TRUE)) ###Combines all matches from all competitions into one dataframe

###we are going to remove a lot of columns to just make our dataset clean
columns.to.keep <- names(which(unlist(lapply(all.matches.df,function(x) length(which(is.na(x)))))==0))

all.matches.clean <- all.matches.df[,columns.to.keep] #this selects the columns by column name 
all.matches.clean$match_week <- as.numeric(all.matches.clean$match_week) #convert some variables to numeric
all.matches.clean$home_score <- as.numeric(all.matches.clean$home_score)
all.matches.clean$away_score <- as.numeric(all.matches.clean$away_score)

####Obtain Events####
event.files <- list.files(path="C:\\Users\\sgopaladesikan\\Desktop\\open-data-master\\data\\events",
                          full.names = TRUE,recursive = TRUE)

event.list <- list()
for(i in 1:length(event.files)){
  event.temp <- fromJSON(file=event.files[i])
  
  #unique(unlist(lapply(event.temp,function(x) x$type$name))) | Let's us see the unique events that happen in a game
  
  teamids <- c() #Get the unique teamids participating in a match
  
  #obtain the index where we find the event that talks about Starting XI
  starting.x11.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Starting XI")
  starting.x11.list <- list()
  for(s in 1:2){
    #we need to remove "jersey_number" from the starting XI because sometimes it is missing and causes recylcing of the matrix
    jersey.number.index <- which(names(unlist(event.temp[[s]]$tactics$lineup))=="jersey_number")
    starting.x11 <- t(unlist(event.temp[[s]]$tactics$lineup)[-which(names(unlist(event.temp[[s]]$tactics$lineup))=="jersey_number")])
    starting.x11.team1 <- data.frame(matrix(starting.x11,ncol=4,byrow = TRUE),stringsAsFactors = FALSE)
    colnames(starting.x11.team1) <- names(unlist(event.temp[[s]]$tactics$lineup)[-which(names(unlist(event.temp[[s]]$tactics$lineup))=="jersey_number")])[1:4]
    starting.x11.team1$formation <- event.temp[[s]]$tactics$formation
    starting.x11.team1$team_id <- event.temp[[s]]$team$id
    
    teamids <- c(teamids,event.temp[[s]]$team$id)
    
    starting.x11.team1$team_name <- event.temp[[s]]$team$name
    starting.x11.list[[s]] <- starting.x11.team1
  }
  
  pass.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Pass")
  
  #obtain the passes just for team1 (the first element in teamids)
  pass.team1 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[1])]
  
  pass.team1.df <- data.frame(matrix(NA,nrow=1,ncol=12))
  colnames(pass.team1.df) <- c("passid","Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part")
  
  for(p in 1:length(pass.team1)){
    pass.temp <- event.temp[[pass.team1[p]]]
    pass.id <- pass.temp$id
    possession <- pass.temp$possession
    passer <- pass.temp$player$name
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- ifelse("recipient" %!in% names(pass.temp$pass),NA,pass.temp$pass$recipient$name)
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(pass.id,possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
    pass.team1.df <- rbind(pass.team1.df,row.toadd)
  }
  pass.team1.df <- pass.team1.df[-1,]
  pass.team1.df[,c(2,4,5,8:11)] <- lapply(pass.team1.df[,c(2,4,5,8:11)],as.numeric)
  
  pass.team1.df <- pass.team1.df %>% group_by(Possession) %>% mutate(seq = row_number())
  pass.team1.df$team_id <- teamids[1]
  
  pass.team2 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[2])]
  pass.team2.df <- data.frame(matrix(NA,nrow=1,ncol=12))
  colnames(pass.team2.df) <- c("passid","Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part")
  
  for(p in 1:length(pass.team2)){
    pass.temp <- event.temp[[pass.team2[p]]]
    pass.id <- pass.temp$id
    possession <- pass.temp$possession
    passer <- pass.temp$player$name
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- ifelse("recipient" %!in% names(pass.temp$pass),NA,pass.temp$pass$recipient$name)
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(pass.id, possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
    pass.team2.df <- rbind(pass.team2.df,row.toadd)
  }
  pass.team2.df <- pass.team2.df[-1,]
  pass.team2.df[,c(2,4,5,8:11)] <- lapply(pass.team2.df[,c(2,4,5,8:11)],as.numeric)
  pass.team2.df <- pass.team2.df %>% group_by(Possession) %>% mutate(seq = row_number())
  pass.team2.df$team_id <- teamids[2]
  
  pass.list <- list(pass.team1.df,pass.team2.df)
  
  shot.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Shot")
  
  #obtain the shots just for team1 (the first element in teamids)
  shots.team1 <- shot.index[which(unlist(lapply(shot.index,function(x) event.temp[[x]]$team$id))==teamids[1])]
  shots.team1.df <- data.frame(matrix(NA,nrow=1,ncol=11))
  colnames(shots.team1.df) <- c("Possession","Player","X.Shot","Y.Shot",
                                "Shot.Type","xG","keypassid","X.KeyPass","Y.KeyPass","Shot.Outcome","Shot.Foot")
  
  if(length(shots.team1)!=0){
    for(p in 1:length(shots.team1)){
      shots.temp <- event.temp[[shots.team1[p]]]
      possession <- shots.temp$possession
      shooter <- shots.temp$player$name
      shots.location <- shots.temp$location
      shots.type <- shots.temp$shot$technique$name
      shots.xg <- ifelse("statsbomb_xg" %!in% names(shots.temp$shot),NA, shots.temp$shot$statsbomb_xg)
      keypass <- ifelse("key_pass_id" %!in% names(shots.temp$shot),NA,shots.temp$shot$key_pass_id)
      keypass.location <- if(!is.na(keypass)){
        as.vector(unlist(pass.team1.df[which(pass.team1.df$passid==keypass),c("X.Pass","Y.Pass")]))
      }else{
        c(NA,NA)
      }
      shots.outcome <- shots.temp$shot$outcome$name
      body.part <- shots.temp$shot$body_part$name
      
      row.toadd <- c(possession,shooter,shots.location,shots.type,shots.xg,keypass,keypass.location[1],keypass.location[2],shots.outcome,body.part)
      shots.team1.df <- rbind(shots.team1.df,row.toadd)
    }
    shots.team1.df <- shots.team1.df[-1,]
    shots.team1.df[,c(1,3,4,6,8,9)] <- lapply(shots.team1.df[,c(1,3,4,6,8,9)],as.numeric)
    shots.team1.df$team_id <- teamids[1]
  }
  
  shots.team2 <- shot.index[which(unlist(lapply(shot.index,function(x) event.temp[[x]]$team$id))==teamids[2])]
  shots.team2.df <- data.frame(matrix(NA,nrow=1,ncol=11))
  colnames(shots.team2.df) <- c("Possession","Player","X.Shot","Y.Shot",
                                "Shot.Type","xG","keypassid","X.KeyPass","Y.KeyPass","Shot.Outcome","Shot.Foot")
  if(length(shots.team2)!=0){
    for(p in 1:length(shots.team2)){
      shots.temp <- event.temp[[shots.team2[p]]]
      possession <- shots.temp$possession
      shooter <- shots.temp$player$name
      shots.location <- shots.temp$location
      shots.type <- shots.temp$shot$technique$name
      shots.xg <- ifelse("statsbomb_xg" %!in% names(shots.temp$shot),NA, shots.temp$shot$statsbomb_xg)
      keypass <- ifelse("key_pass_id" %!in% names(shots.temp$shot),NA,shots.temp$shot$key_pass_id)
      keypass.location <- if(!is.na(keypass)){
        as.vector(unlist(pass.team1.df[which(pass.team1.df$passid==keypass),c("X.Pass","Y.Pass")]))
      }else{
        c(NA,NA)
      }
      shots.outcome <- shots.temp$shot$outcome$name
      body.part <- shots.temp$shot$body_part$name
      
      row.toadd <- c(possession,shooter,shots.location,shots.type,shots.xg,keypass,keypass.location[1],keypass.location[2],shots.outcome,body.part)
      shots.team2.df <- rbind(shots.team2.df,row.toadd)
    }
    shots.team2.df <- shots.team2.df[-1,]
    shots.team2.df[,c(1,3,4,6,8,9)] <- lapply(shots.team2.df[,c(1,3,4,6,8,9)],as.numeric)
    shots.team2.df$team_id <- teamids[2]
  }
  
  shot.list <- list(shots.team1.df,shots.team2.df)
  
  match.id <- strsplit(basename(event.files[i]),"[.]")[[1]][1]
  
  event.list[[match.id]] <- list(starting.x11.list,pass.list,shot.list)
  
}


##########Create a Shiny Application That Uses Shot Information##############
#Already I have found some mistakes in pulling in shot information, can you find them?

#We want to create a dataframe of all shots with meta-information (e.g. competition, season, pass location, shot location, shooter)

shots <- lapply(event.list,function(x) x[[3]]) #we want to just take the shots information from the event.list list
shots.df <- plyr::ldply(lapply(shots,function(x) rbindlist(x,fill=TRUE)),.id="match_id") #we want to combine both teams' shots within a game into a datamframe, and then combine all matches together using match_id as a new column 
shots.df <- shots.df %>% group_by(match_id,team_id) %>% mutate(xGoal_Total = sum(xG)) #we are adding the sum of all shots' xG to give each team a xG_Total (how many goals were they expected to score given their shot quality)

#This is merging together the shots with the matches to get match information
shots.shiny <- merge(shots.df,all.matches.df[,c("match_id","competition.competition_name","season.season_name",
                                                "home_score","away_score","home_team.home_team_id","away_team.away_team_id",
                                                "home_team.home_team_name","away_team.away_team_name")],by="match_id") #some matches are missing

#This is calculating the TeamScore for the team_id column so that I can use this in calculating the difference between actual goals and expected goals for an entire match for a team
shots.shiny$TeamScore <- ifelse(shots.shiny$team_id==shots.shiny$home_team.home_team_id,
                                as.numeric(shots.shiny$home_score),
                                as.numeric(shots.shiny$away_score))

#This is the calculation for calculating the difference between actual and expected
shots.shiny$xG_Diff <- shots.shiny$TeamScore - shots.shiny$xGoal_Total

#The next three lines just gives me the an additional column to the shots dataframe to let me know which team took the shot
teams.df <- unique(all.matches.df[,c("home_team.home_team_id","home_team.home_team_name")])

shots.shiny.final <- merge(shots.shiny,teams.df,by.x="team_id",by.y="home_team.home_team_id")
colnames(shots.shiny.final)[25] <- "team_name"

#Adding a column to let me know if a shot was assisted or not
shots.shiny.final$isAssisted <- ifelse(is.na(shots.shiny.final$keypassid),FALSE,TRUE)



############Create a Simple Shiny Application#############################
source("your_file_path\\Draw_Pitch.R") #load in hori5, which contains the soccer field

#install.packages("shiny")
library(shiny)
#runExample()
#runExample("01_hello") 

# Define UI for app that draws a shot map ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of shots ----
      sliderInput(inputId = "shots",
                  label = "Number of shots:",
                  min = 1,
                  max = nrow(shots.shiny.final),
                  value = nrow(shots.shiny.final))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Shot Map ----
      plotOutput(outputId = "shotPlot")
      
    )
  )
)

# Define server logic required to draw a shot map ----
server <- function(input, output) {
  
  # Shot Map of Statsbomb Data----
  # with requested number of shots
  # This expression that generates a shot map is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$shots) change
  # 2. Its output type is a plot
  output$shotPlot <- renderPlot({
    
    x    <- sample(1:nrow(shots.shiny.final),input$shots)
    
    hori5 + geom_point(data=shots.shiny.final[x,], aes(x=X.Shot,y=Y.Shot,color=xG))
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
