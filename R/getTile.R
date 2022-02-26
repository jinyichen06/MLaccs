getTile=function(ScoreFile,ScoredVariable,BestTile1=FALSE,TileNum=10){
  #ScoreFile is a data frame created in Scoring function.

  ### convert to data.table
  setDT(ScoreFile)

  if(!(BestTile1 %in% c(TRUE,FALSE))){
    stop("The BestTile1 can only be a logical indicating whether the tile 1 corresponds to the highest Scores", call. = FALSE)
  }

  # N_T=dim(ScoreFile)[1]
  # ScoreFile[,ID_temp:=1:N_T]

  #-------------------------------------- create Bins and Tiles -----------------------------------------------------------------------------------------------
  bins <- TileNum
  q <- quantile(ScoreFile[[ScoredVariable]], probs=c(0:(bins)/bins), na.rm=TRUE, type=3)
  cuts <- unique(q)
  cuts_Len <- length(cuts)

  if(cuts_Len==1){
    stop("All the scores are the same", call. = FALSE)
  }

  while (cuts_Len < TileNum+1) {
    q <- quantile(ScoreFile[[ScoredVariable]], probs=c(0:(bins)/bins), na.rm=TRUE, type=3)
    cuts <- unique(q)
    cuts_Len <- length(cuts)
    bins <- bins + 1
  }

  if (is.character(ScoreFile[[ScoredVariable]])==FALSE & is.factor(ScoreFile[[ScoredVariable]])==FALSE){
    Score_bin <- cut(ScoreFile[[ScoredVariable]], cuts, right=FALSE, include.lowest = TRUE, dig.lab=4)
    Tile <- cut(ScoreFile[[ScoredVariable]], cuts, right=FALSE, include.lowest = TRUE,labels=FALSE)
    Score_bin <- as.character(Score_bin)
    Score_bin[is.na(Score_bin)]<-""
  } else{
    Score_bin <- ScoreFile[[ScoredVariable]]
  }

  Tile_Unique <- sort(unique(Tile))
  Tile_Len <- length(Tile_Unique)
  Tile_Unique_df <- data.frame(Tile=Tile_Unique,Tile_rev=rev(Tile_Unique))

  if(BestTile1){
    Tile_df <- data.table(Tile=Tile)
    Tile_df <- left_join(Tile_df,Tile_Unique_df,by="Tile")
    Tile <- Tile_df[["Tile_rev"]]
  }

  #-------------------------------------- add Bins and Tiles to the ScoreFile --------------------------------------------------------------------------
  ScoreFile[,c("Score_bin","Tile"):=list(Score_bin, Tile)]
  return(ScoreFile)
}# end of function getTile
