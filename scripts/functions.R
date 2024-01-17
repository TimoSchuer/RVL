## collapse IP
collapseIP <- function(exb,coerce_cols="Text", annotation_dummies=FALSE, annotation_cols=c() ){
  if(annotation_dummies==TRUE & length(annotation_cols)!=0){
    ann <- exb %>%
      mutate(across(annotation_cols, ~tidyr::replace_na(.x,"NA"))) %>%
      mutate(across(annotation_cols, ~as.factor(.x))) %>%
      pivot_longer(cols = annotation_cols,
                   names_to = "dummy_names",
                   values_to = "dummy_levels") %>%
      mutate(dummy_value = 1) %>%
      pivot_wider(id_cols= c(IPId,EventID),
                  names_from = c(dummy_names, dummy_levels),
                  values_from = dummy_value,
                  values_fill = 0) %>%
      select(-EventID) %>%
      summarise(across(everything(), ~sum(.x)), .by=IPId)
    exb <- exb %>%
      summarise(IPId=first(IPId),
                IPNumber= first(IPNumber),
                File=first(File),
                Speaker=first(Speaker),
                TierID= first(TierID),
                TierCategory=first(TierCategory),
                Start=first(Start),
                End=last(End),
                Name=first(Name),
                Start_time=min(Start_time),
                End_time=max(End_time),
                across(all_of(coerce_cols),~paste(.x,collapse=" ")), .by = IPId )%>%
      left_join(ann, by="IPId")
    return(exb)

  }
  exb <- exb %>%
    summarise(IPId=first(IPId),
              IPNumber= first(IPNumber),
              File=first(File),
              Speaker=first(Speaker),
              TierID= first(TierID),
              TierCategory=first(TierCategory),
              Start=first(Start),
              End=last(End),
              Name=first(Name),
              Start_time=min(Start_time),
              End_time=max(End_time),
              across(all_of(coerce_cols),~paste(.x,collapse=" ")), .by = IPId )

  return(exb)
}
#createKwic
createKWIC <- function(exb_tok = data.frame(),
                       window = 2,
                       pattern = "",
                       textCol = "Text",
                       summarise_pattern = FALSE,
                       IPintern=TRUE) {

  if (window > length(exb_tok[[textCol]])) {
    stop("Error: window size is greater than the length of the text column.")
  }

  # Split the pattern into individual words
  pattern_words <- stringr::str_split(pattern, " ")[[1]]
  Text <- exb_tok %>%
    mutate(Text= str_remove_all(Text,"\\W") %>% str_trim()) %>%
    pull(Text)
  searchMat <-    sapply(
    1:length(Text),
    \(x) paste(Text[x:min(x+length(pattern_words)-1, nrow(exb_tok))], collapse = " ")
  )


  # Find the positions where the pattern matches the text
  exb_tok <- exb_tok %>% mutate(rowNumber= row_number())
  match_positions <- which(searchMat==pattern)
  result <- data.frame()
  i <- 1
  for (k in match_positions) {
    rows <- seq(k-window, k +length(pattern_words)-1+window)
    if(min(rows)<0){
      rows <- rows[which(rows>=0)]
    }
    if(max(rows>nrow(exb_tok))){
      rows <- rows[which(rows<=nrow(exb_tok))]
    }
    match <- exb_tok[rows,] %>%
      mutate(BelegNr=i) %>%
      mutate(pos=case_when(rowNumber %in% seq(k, k+length(pattern_words)-1)~ 0,
                           .default = NA))
    result <- bind_rows(result, match)
    i <- i+1
  }
  matches <- c()
  for (t in match_positions) {
    matches <- c(matches, seq(t,t+length(pattern_words)-1))
  }
  # Set the pos column to 0 for the matching positions
  result <-  result %>%
    #    mutate(pos = if_else(rowNumber %in% matches, 0, NA)) %>%
    group_by(BelegNr,pos) %>%
    mutate(MatchPos=if_else(pos==0,row_number(),NA)) %>%
    ungroup() %>%
    mutate(RNumber= row_number(), .by= BelegNr) %>%
    mutate(pos= case_when(is.na(MatchPos)& RNumber<=RNumber[which(MatchPos==1)]~ RNumber-RNumber[which(MatchPos==1)],
                          !is.na(MatchPos)~0,
                          is.na(MatchPos)& RNumber>=RNumber[which.max(MatchPos)]~RNumber-RNumber[which.max(MatchPos)]), .by = BelegNr)

  if(IPintern){
    result <- result %>%
      dplyr::filter(IPId==IPId[which(MatchPos==1)], .by=BelegNr)
  }
  # Optionally summarise the pattern matches
  if (summarise_pattern) {
    pattern_summary <- paste(pattern_words, collapse = " ")
    exb_tok <- exb_tok %>%
      distinct_at(vars(textCol, pos)) %>%
      mutate(Text = if_else(pos == 0, pattern_summary, Text))
  }

  result <- result %>% select(!c(rowNumber,MatchPos,RNumber))
  # Return the modified data frame
  return(result)

}
#sendPraat
#' @param data data.frame created bei ExmaraldaR with function readExb has to contain the columns "pathAudio", "Start_time" and "End_time"
#' @param rowNumber row number of the data frame to be sent to Praat
#' @param pathPraat path to the folder containing the Praat executable 'sendpraat-win.exe' ; note that the path has to be in quotation marks and should not end with a slash
sendPraat <- function(data=data.frame(),rowNumber=1, pathPraat=character()){
  stopifnot(length(pathPraat)==1)
  paste(pathPraat,"/sendpraat-win.exe praat ",
        paste0("\"Open long sound file: \\\"",data[rowNumber, "pathAudio"],"\\\"\"",sep=""), sep="") %>%
    system()
  paste0(pathPraat,'/sendpraat-win.exe praat ',
         '"selectObject: ', '\\"LongSound ',
         data[rowNumber,] %>%
           dplyr::pull(pathAudio) %>%
           stringr::str_extract("/[^/]*\\.") %>%
           stringr::str_remove_all("\\.") %>%
           stringr::str_remove_all("/") ,'\\""',
         " \"View\"",
         paste(" \"editor: ", '\\"LongSound ',
               data[rowNumber,] %>%
                 dplyr::pull(pathAudio) %>%
                 stringr::str_extract("/[^/]*\\.") %>%
                 stringr::str_remove_all("\\.") %>%
                 stringr::str_remove_all("/") ,
               '\\""' ,

               sep=""),
         paste(" \"Zoom: ",data[rowNumber, "Start_time"]-3,", ",data[rowNumber, "End_time"]+3,"\"",sep=""),
         " endeditor",
         sep="") %>% system()

}
#showTranscript
#' @param data data
#' @param corpus corpus
#' @param rowNumber row from data
#' @param tier tier to show transkript
#'
#' @return
#' @export
#'
#' @examples

showTranscript <- function(data= data.frame(),corpus=data.frame(),rowNumber=1, tier=c("beides","Standard","Transkript")){
  if(length(tier)>1){
    tier <- "beides"
  }
  selectedIPId <- data[rowNumber,] %>% pull(IPId)
  selected <- which(corpus$IPId==selectedIPId)
  if(length(selected)==0){
    return(data.frame(error="Fehler bei der Transkriptanzeige"))
  } else if(length(selected)>1){
    selected <- selected[1]
  }
  if(selected<=20){
    selected <- 21
  }
  if(tier=="beides"){
    if(selected<=10){
      selected<- 11
    }
    transcript <- corpus %>%
      dplyr::select(IPId,Name,Text) %>%
      dplyr::slice(seq(selected-10,selected+10))
  }else if(tier=="Standard" &
           stringr::str_detect(corpus[selected,"File"],"Weg")){
    transcript <- corpus %>%
      dplyr::slice(seq(selected-20,selected+20)) %>%
      dplyr::filter(TierCategory=="min") %>%
      dplyr::select(IPId,Name,Text)
  }else if(tier=="Standard" &
           stringr::str_detect(corpus[selected,"File"],"Dia|WEBER")){
    transcript <- corpus%>%
      dplyr::slice(seq(selected-20,selected+20)) %>%
      dplyr::filter(TierCategory=="SD"|TierCategory=="Übersetzungsspur") %>%
      dplyr::select(IPId,Name,Text)
  }else if(tier=="Transkript" &
           stringr::str_detect(corpus[selected,"File"],"Weg")&
           corpus %>%
           dplyr::filter(File==corpus[1,"File"]) %>%
           dplyr::filter(TierCategory=="bas") %>%
           nrow()> 0){
    selected_start <-  which(corpus$Start_time==corpus[selected,"Start_time"]&
                               corpus$TierCategory=="bas" &
                               corpus$File== corpus[selected,"File"])
    selected_end <- which(corpus$End_time==corpus[selected,"End_time"]&
                            corpus$TierCategory=="bas"&
                            corpus$File== corpus[selected,"File"])
    selected_start <- intersect(selected_start,selected_end)
    if(selected_start %>% length()==0){
      transcript <- corpus %>%dplyr::select(IPId,Name,Text) %>% dplyr::slice(seq(selected-10,selected+10))
    }else{
      transcript <- corpus %>%dplyr::slice(seq(selected_start-20,selected_start+20)) %>%
        dplyr::filter(TierCategory=="bas") %>%
        dplyr::select(IPId,Name,Text)

    }
  }else if(tier=="Transkript" &
           stringr::str_detect(corpus[selected,"File"],"Weg")&
           corpus %>%
           dplyr::filter(File==corpus[1,"File"]) %>%
           dplyr::pull(TierCategory) %>%
           unique() %>%
           dplyr::intersect("bas") %>%
           length()==0){
    transcript <- corpus %>%
      dplyr::select(IPId,Name,Text) %>%
      dplyr::slice(seq(selected-10,selected+10))
  }else if(tier=="Transkript" &
           stringr::str_detect(corpus[selected,"File"],"Dia|WEBER")){
    selected_start <-  which(corpus$Start_time==corpus[selected,"Start_time"]&
                               (corpus$TierCategory=="ND"| corpus$TierCategory=="platt")&
                               corpus$File== corpus[selected,"File"])
    selected_end <- which(corpus$End_time==corpus[selected,"End_time"]&
                            (corpus$TierCategory=="ND"| corpus$TierCategory=="platt")&
                            corpus$File== corpus[selected,"File"])
    selected_start <- intersect(selected_start,selected_end)
    selected_start <- selected_start[1]

    if(selected_start %>% length()==0){
      transcript <- corpus %>%dplyr::select(IPId,Name,Text) %>% dplyr::slice(seq(selected-10,selected+10))
    }else{
      transcript <- corpus %>%
        dplyr::slice(seq(selected_start-20,selected_start+20)) %>%
        dplyr::filter(TierCategory=="ND"|TierCategory=="platt") %>%
        dplyr::select(IPId,Name,Text)

    }

  }
  if(exists("transcript")==FALSE){
    return(data.frame(error="Fehler bei der Transkriptanzeige"))
  }
  return(transcript)
}

#' tokenize_exb
#'
#' @param x data.frame from read_exb
#' @param assign_times assign time
#' @param transcription_text text column that should be split
#'
#' @return data.frame
#' @export
#'
#' @examples
tokenize_exb <- function(x, assign_times=TRUE, transcription_text= "Text"){
  if(assign_times==FALSE){
    tokenized <- x %>%
      tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]])
  }else if(assign_times==TRUE){
    maxIP <- x %>% group_by(IPId) %>% mutate(IPn=cur_group_id()) %>% pull(IPn) %>% max()
    if(x %>%
       tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]]) %>%
       dplyr::mutate(TextIP=  paste0(Text, collapse = " "), .by = IPId) %>%
       dplyr::mutate(IPn= cur_group_id()) %>%
       dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
       filter(TimePerToken<=0.05) %>% nrow()>0){
      cat("There are some problems in the transcripts. Events are too short. Check returned data.frame")
      # return(x %>%
      #          tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]]) %>%
      #          dplyr::mutate(TextIP=  paste0(Text, collapse = " "), .by = IPId) %>%
      #          dplyr::mutate(IPn= cur_group_id()) %>%
      #          dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
      #          filter(TimePerToken<=0.05))
    }


    tokenized<-  x %>%
      tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]]) %>%
      dplyr::mutate(TextIP=  paste0(Text, collapse = " "), .by = IPId) %>%
      dplyr::mutate(IPn= cur_group_id()) %>%
      dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
      dplyr::mutate(Start_time_IP= min(Start_time), .by = IPId) %>%
      dplyr::mutate(End_time_IP= max(End_time), .by = IPId) %>%
      dplyr::mutate(length_token= (End_time_IP- Start_time_IP) /n(), .by= IPId) %>%
      dplyr::mutate(Start_time= if_else(row_number()==1, Start_time_IP, Start_time_IP + cumsum(length_token)- length_token),.by = IPId) %>%
      dplyr::mutate(End_time = if_else(row_number()==1, Start_time_IP + length_token,Start_time_IP+cumsum(length_token)), .by = IPId) %>%
      dplyr::mutate(TokenID= row_number() %>% paste(IPId,., sep =  "_"), .by = IPId) %>%
      dplyr::mutate(Start_time=round(Start_time,6)) %>%
      dplyr::mutate(End_time=round(End_time,6))
    #select(-c(length_token, Start_time_IP,End_time_IP,IPn))
    ##timeline konsistent machen
    #Durch die Anapassung der Start und Endzeitpunkte kann es vorkommen, dass der Start des nächsten Ereignisses vor dem Ende des vorherigen liegt,
    #das lässt sich anpassen indem man den endzeitpunkt des vorherigen als Startzeitpunkt des nächsten nimmt.
    #In wenigen Fällen führt dies dazu dass dieses Ereignis nun nicht mehr passt diese Ereignisse muss man aussortieren


    # t <- data.frame()
    # for (sp in unique(x$Name)) {
    #   h <- tokenized %>% filter(Name==sp) %>%
    #     dplyr::mutate(Start_time=if_else(Start_time< lag(End_time)& row_number()!=1 & End_time- lag(End_time)>=0, lag(End_time),Start_time)) %>%
    #     dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
    #     dplyr::mutate(Start_time_conf= if_else(lag(End_time)> Start_time& !is.na(lag(End_time)), TRUE, FALSE)) %>%
    #     dplyr::mutate(tok_length_conf=if_else(End_time - Start_time < 0.2, TRUE, FALSE))
    #   ##lässt einige Fehler übrigen, irgendwann mach ich das ordentlich...
    #   t <- bind_rows(t,h)
    # }
    #tokenized <- t %>% arrange(IPId) %>% select(-c(IPn,TextIP,TimePerToken,TimePerToken_after, Start_time_IP, End_time_IP,length_token,Start_time_conf, tok_length_conf))
  }
  return(tokenized)
}

