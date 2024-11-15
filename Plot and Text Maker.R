library(dplyr)
library(plotly)
library(stringr)
library(stringi)
library(compare)
library(httr)
library(jsonlite)
library(htmlwidgets)

apiKey <- "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"
Sys.setenv(chatGPT_API_KEY = apiKey)
chatGPT <- function(prompt, 
                    modelName = "gpt-3.5-turbo",
                    temperature = 1,
                    apiKey = Sys.getenv("chatGPT_API_KEY")) {
  
  if(nchar(apiKey)<1) {
    apiKey <- readline("Paste your API key here: ")
    Sys.setenv(chatGPT_API_KEY = apiKey)
  }
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", apiKey)),
    content_type_json(),
    encode = "json",
    body = list(
      model = modelName,
      temperature = temperature,
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    )
  )
  
  if(status_code(response)>200) {
    stop(content(response))
  }
  
  trimws(content(response)$choices[[1]]$message$content)
}


week <- as.numeric(readline('What week are we scraping? (only number)'))

history <- read.csv('D:/Fantasy Hockey/Matchup_History.csv')


output_dir <- file.path('D:/Fantasy Hockey/', paste0('Week',week))

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

history <- history[history$V5 == paste("Week",week),]

for(i in 1:nrow(history)){
  output_dir <- file.path('D:/Fantasy Hockey/', paste0('Week',week),paste0(history[i,1],'_',history[i,3]))
  
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  }
}

skaters <- read.csv(paste0("D:/Fantasy Hockey/week",week,"skaters.csv"))
skaters$Day <- as.Date(skaters$Day,'%m/%d')
goalies <- read.csv(paste0("D:/Fantasy Hockey/week",week,"goalies.csv"))
goalies$Day <- as.Date(goalies$Day,'%m/%d')

for(i in 1:nrow(skaters)){
  temp <- str_split(skaters[i,2]," - ")[[1]][1]
  skaters[i,2] <- sub("[A-Z/]+$", "", temp)
}
for(i in 1:nrow(goalies)){
  temp <- str_split(goalies[i,2]," - ")[[1]][1]
  goalies[i,2] <- sub("[A-Z/]+$", "", temp)
}


for(i in 1:nrow(history)){
  
  team1 <- history[i,1]
  score1 <- history[i,2]
  team2 <- history[i,3]
  score2 <- history[i,4]
  
  match_skaters <- skaters[skaters$Team == team1 | skaters$Team == team2,]
  match_goalies <- goalies[goalies$Team == team1 | goalies$Team == team2,]
  ############# Make 3 stars
  
  skater_match_totals <- match_skaters %>% group_by(Player, Team) %>% summarize(Fan.Pts=sum(Fan.Pts), G=sum(G),A=sum(A),PIM=sum(PIM),
                                                                                PPP=sum(PPP),SHP=sum(SHP),GWG=sum(GWG),SOG=sum(SOG),FW=sum(FW),
                                                                                FL=sum(FL), HIT=sum(HIT),BLK=sum(BLK))
  skater_match_totals <- skater_match_totals[order(skater_match_totals$Fan.Pts, decreasing = T),]
  totals_team1 <- skater_match_totals[skater_match_totals$Team == team1,]
  totals_team2 <- skater_match_totals[skater_match_totals$Team == team2,]
  
  goal_match_total <- match_goalies %>% group_by(Player, Team) %>% summarize(Fan.Pts=sum(Fan.Pts),GS=sum(GS),W=sum(W),GA=sum(GA),SV=sum(SV),SHO=sum(SHO))
  goal_match_total <- goal_match_total[order(goal_match_total$Fan.Pts, decreasing = T),]
  totals_team1_goal <- match_goalies[match_goalies$Team == team1,]
  totals_team2_goal <- match_goalies[match_goalies$Team == team2,]
  all_totals_team1 <- rbind(totals_team1[,c(1,3)],totals_team1_goal[,c(2,3)]) %>% arrange(desc(Fan.Pts))
  all_totals_team2 <- rbind(totals_team2[,c(1,3)],totals_team2_goal[,c(2,3)]) %>% arrange(desc(Fan.Pts))
  all_totals_team1 <- all_totals_team1 %>% group_by(Player) %>% summarise(Fan.Pts=sum(Fan.Pts)) %>% arrange(desc(Fan.Pts))
  all_totals_team2 <- all_totals_team2 %>% group_by(Player) %>% summarise(Fan.Pts=sum(Fan.Pts)) %>% arrange(desc(Fan.Pts))
  
  ##########################################################################
  ###
  ###                             Three Stars
  ###
  ##########################################################################
  three_stars <- data.frame(matrix(NA,nrow=5,ncol=5))
  colnames(three_stars) <- c("Fan.Pts","Away","Star","Home","Fan.Pts")
  three_stars[,1] <- all_totals_team1[1:5,"Fan.Pts"]
  three_stars[,2] <- all_totals_team1[1:5,"Player"]
  three_stars[,3] <- c('First Star','Second Star','Third Star','Fourth Star','Fifth Star')
  three_stars[,4] <- all_totals_team2[1:5,"Player"]
  three_stars[,5] <- all_totals_team2[1:5,"Fan.Pts"]
  three_stars <- plot_ly(
    type = 'table',
    columnorder = c(1:5),
    columnwidth = c(40,80,80,80,40),
    header = list(
      values = c('Fan.Points','Away', 'Star','Home','Fan.Points'),
      line = list(color = '#506784'),
      fill = list(color = '#119DFF'),
      align = c('center','center'),
      font = list(color = 'white', size = 12),
      height = 40
    ),
    cells = list(
      values = as.matrix(t(three_stars)),
      line = list(color = '#506784'),
      fill = list(color = c('white','white','#25FEFD', 'white','white')),
      align = c('left', 'center'),
      font = list(color = c('#506784'), size = 12),
      height = 30
    ))
  prompt <- paste0('Give insights on these tables of top players from a fantasy hockey matchup. The first table has columns: Player, fantasy team, fnatasy points, goals, assists, penalties in minutes, power play points, short handed points, game winning goals, shots on goal, faceoffs won, faceoffs lost, hits, blocks.
                 The second table has columns: Player, fantasy team, fantasy points, game started, wins, goals against, saves, shutouts. Try to stick to the best players by fantasy points, but include people on both fantasy teams and from both tables. 
                 Dont tell me what the tables show, I already know. But point out impressive statistics for players. Make the output a paragraph for each fantasy team', toString(skater_match_totals[1:10,]), toString(goal_match_total[1:3,]))
  stars <- chatGPT(prompt)
  save<- file.path('D:', 'Fantasy Hockey', paste0('Week',week), paste0(history[i,1],'_',history[i,3]))
  htmlwidgets::saveWidget(as_widget(three_stars), paste0(save,'\\threestars.html'))
  write.csv(stars,paste0(save,'\\starts_text.txt'))

  ##########################################################################
  ###
  ###                             Score Timeline
  ###
  ##########################################################################
  ### Create overall timelines
  timeline <- match_skaters %>% group_by(Day,Team) %>% summarise(Fan.Pts=sum(Fan.Pts))
  timeline_g <- match_goalies %>% group_by(Day,Team) %>% summarise(Fan.Pts=sum(Fan.Pts))
  timeline <- rbind(timeline, timeline_g)
  rm(timeline_g)
  
  ### Split by team
  time_1 <- timeline[timeline$Team == team1,] %>% group_by(Day, Team) %>% summarize(Fan.Pts = sum(Fan.Pts)) %>% arrange(Day) 
  time_2 <- timeline[timeline$Team == team2,]  %>% group_by(Day, Team) %>% summarize(Fan.Pts = sum(Fan.Pts)) %>% arrange(Day)
  ### Make sure both ts's have the same days
  while( length(time_1$Day[!(time_1$Day %in% time_2$Day)]) != 0 | length(time_2$Day[!(time_2$Day %in% time_1$Day)]) != 0){
    time_1$Day <- as.character(time_1$Day)
    time_2$Day <- as.character(time_2$Day)
    time_1$Fan.Pts <- as.character(time_1$Fan.Pts)
    time_2$Fan.Pts <- as.character(time_2$Fan.Pts)
    if (length(time_1$Day[!(time_1$Day %in% time_2$Day)]) != 0){
      time_2[nrow(time_2)+1,] <- t(c(as.character(time_1$Day[!(time_1$Day %in% time_2$Day)][1]),team2,0))
    }
    if(length(time_2$Day[!(time_2$Day %in% time_1$Day)]) != 0){
      time_1[nrow(time_1)+1,] <- t(c(as.character(time_2$Day[!(time_2$Day %in% time_1$Day)][1]),team1,0))
    }
  }
  
  ### Rearrange and make correct data types
  time_1$Day <- as.Date(time_1$Day)
  time_2$Day <- as.Date(time_2$Day)
  time_1$Fan.Pts <- as.double(time_1$Fan.Pts)
  time_2$Fan.Pts <- as.double(time_2$Fan.Pts)
  time_1 <- time_1 %>% arrange(Day)
  time_2 <- time_2 %>% arrange(Day)
  time_2$Total <- cumsum(time_2$Fan.Pts)
  time_1$Total <- cumsum(time_1$Fan.Pts)

  
  date_points <- plot_ly(ungroup(time_1),x=~Day) %>%
    add_lines(y = ~Total, name = team1,
              line = list(color = 'red', width = 2),
              yaxis = "y") %>%
    add_lines(y = ~time_2$Total, name=team2,
              line = list(color = 'blue', width = 2),
              yaxis = "y")
  
  prompt_days <- paste0('Given these two tables showing the number of fantasy points scored each day over a weekly matchup, tell me insights about the ebb and flow
                      of the matchup. Do not tell me about the range of variance. 
                      The dates are in year-month-day format. Both tables have columns Date, fantasy team, fantasy points on that day, and cumulative fantasy points.', toString(time_1[,2:4]), toString(time_2[,2:4]),
                        'Both tables are in order over the days: ', toString( time_1_string), 'Look for either high scoring days from column 2, or leader changes from column 3, or point out days where one team drastically outscored the other. 
                      Make the output follow the order by day. When printing the dates, skip the year and show the month as the appropriate three letter shorthand.')
  days <- chatGPT(prompt_days)
  htmlwidgets::saveWidget(as_widget(date_points), paste0(save,'\\timeline.html'))
  write.csv(days,paste0(save,'\\timeline_text.txt'))
  
  ##########################################################################
  ###
  ###                             Position Points
  ###
  ##########################################################################
  position_points <- match_skaters %>% group_by(Pos,Team) %>% summarise(Fan.Pts = sum(Fan.Pts))
  position_points <- rbind(position_points, match_goalies %>% group_by(Pos,Team) %>% summarise(Fan.Pts=sum(Fan.Pts)))
  position_points_1 <- position_points[position_points$Team == team1,] %>% arrange(match(Pos, c("C", "LW", "RW","D","G")))
  position_points_2 <- position_points[position_points$Team == team2,] %>% arrange(match(Pos, c("C", "LW", "RW","D","G")))
  
  xform <- list(categoryorder = "array",
                categoryarray = position_points_1$Pos)
  
  pos_points <- plot_ly(position_points_1, x = ~Pos, y = ~Fan.Pts, type = 'bar', name = team1,marker = list(color = 'red')) %>%
    add_trace(y = ~position_points_2$Fan.Pts, name = team2,marker = list(color = 'blue')) %>%
    layout(title = "Position Points",
           xaxis = xform)
  
  prompt <- paste0('Give insights into the following table representing fantasy hockey points over a matchup based on position.', toString(position_points),
                   'Dont describe the table or tell me about every position, focus on where one team had a large advantage over the other, and dont mention the table')
  position_text <- chatGPT(prompt)
  htmlwidgets::saveWidget(as_widget(pos_points), paste0(save,'\\positions.html'))
  write.csv(position_text,paste0(save,'\\positions_text.txt'))
  
}





