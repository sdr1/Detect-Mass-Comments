##############################################################################
#                                                                            #
#.           Document Similarity for Large Numbers of Documents              #
#                                                                            #
##############################################################################

library(doParallel)
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(magrittr)
library(igraph)

# wrap the time consuming part of the function - the similar text, in a parallel wrapper
simil_parallel_wrapper <- function(cmts_dfm, df, sim_method, threshold, seq_length){
  
  # how many observations
  df_len <- dim(df)[1]
  
  # get beginning index for chunks we're going to feed to the parallel 
  # process
  begin1 <- seq(from = 1, to = df_len, by = seq_length)#[1:7]
  
  # total number of iterations
  ln <- length(begin1)
  
  # begin parallel loop
  foreach(i = 1:ln) %dopar% {
    #require(quanteda)
    require(quanteda.textstats)
    require(dplyr)
    require(magrittr)
    
    cat(c(paste("On iteration: ", i, " of", ln)), sep = "\n")
    
    if(i == ln){
      # on the final iteration do this 
      simil_matrix <- quanteda.textstats::textstat_simil(cmts_dfm, y = cmts_dfm %>% magrittr::extract(begin1[i]:df_len,), margin = "documents", method = sim_method, min_simil = threshold)
    } else {
      # on other iterations go from the first part of the sequence to the one before the next sequence
      # suppose you're doing sequences of 10, this goes from 1 to 9 on the first, 10:19 on the second, etc...
      simil_matrix <- quanteda.textstats::textstat_simil(cmts_dfm, y = cmts_dfm %>%  magrittr::extract(begin1[i]:(begin1[i+1]-1),), margin = "documents", method = sim_method, min_simil = threshold)
    }
  }
}

find_near_duplicates <- function(df, doc_text, doc_name, sim_method = "cosine", 
                                 threshold = 0.9, clstrs = 2,walktrap=F, 
                                 seq_length = 10, token_level = "word", ngram = F, ngram_length = 10, ...){
  # token_level needs to be = c(word, character, sentence)  
  
  df <- df %>%
    dplyr::select(all_of(doc_name), all_of(doc_text)) %>%
    dplyr::rename("doc_name" = 1, "comment_text" =2) 
  
  # get rid of duplicated comments
  df <- df[!duplicated(df),]
  
  # quanteda hates duplicated document names, so get rid of duplicated document names
  if(length(which(duplicated(df$doc_name) | duplicated(df$doc_name, fromLast = T))) != 0){
    cat("Duplicates!", filter_value, df$doc_name[duplicated(df$doc_name)], "THIS HAS FAILED, FIX IT!")
    df <- df[!duplicated(df$doc_name),]
  }
  
  # setup corpus
  cmt_corpus <- quanteda::corpus(x = df$comment_text,docnames = df$doc_name)
  
  # convert the corpus to a document feature matrix via tokens
  cmt_token <- quanteda::tokens(cmt_corpus,what = token_level)
  
  if(missing(ngram) | missing(ngram_length)){
    print("No Ngram")
  } else {
    cmt_token <- quanteda::tokens_ngrams(cmt_token,n = ngram_length)
  }
  # convert to document feature matrix
  cmts_dfm <- quanteda::dfm(cmt_token)
  
  # get time started (you may need to adjust the seq_length if this is taking too long)
  ptm <- proc.time()
  cl <- parallel::makeCluster(clstrs, outfile = "")
  doParallel::registerDoParallel(cl)
  getDoParWorkers()
  list_of_similarities <- tryCatch(simil_parallel_wrapper(cmts_dfm, df, sim_method, threshold, seq_length), error = function(e) print(e))
  print(proc.time() - ptm)
  #showConnections(all=TRUE)

  # put all the similarities together  
  similarities <- do.call(cbind,list_of_similarities )
  
  # #process data and put it in a data frame to manipulate it
  # # B is the same as B and A
  # similarities <- Matrix::tril(similarities)
  
  # set diagonal similarity to 0 because we know C is like C
  diag(similarities) <- 0
  
  # Need to do the double conversion from matrix to data.frame to make this work
  similarities <- as.matrix(similarities)
  
  similarities <- data.frame(similarities)
  
  # replace colnames with rownames so the names are the same.
  # since matrix is symmetric.  coercing to a data frame screws up 
  # the column names, so replace them with the originals 
  colnames(similarities) <- rownames(similarities)
  
  # generate a T/F for whether there are comments with a similarity score greater than 0.9 
  # this will be by both Column and Row because the matrix is symmetric
  # now subset the data frame to only get pairs of similar comments 
  
  # if you subset to just an upper diagonal matrix, you need the following line
  # output_matrix <- similarities[sort(unique(unlist(sapply(similarities, function(x){which(x>threshold)})))),
  #                               sort(sapply(similarities, function(x){any(x>threshold)}))]

  # fixing an issue with the subset in hte next step
  similarities[is.na(similarities)] <- 0
                                   
  output_matrix <- similarities[apply(similarities, MARGIN = 1, function(x){any(x>threshold)}),
                                apply(similarities, MARGIN = 2, function(x){any(x>threshold)}),]
  
  # Extract row and column names where elements > 0.9
  indices <- which(output_matrix > threshold, arr.ind = T)
  
  # Extract row and column names
  row_names <- rownames(output_matrix)[indices[, 1]]
  col_names <- colnames(output_matrix)[indices[, 2]]
  
  # Combine row and column names
  result <- data.frame(row_names, col_names)
  
  # Sort the rows so they're always alphabetical
  sorted_result <- data.frame(t(apply(result, 1, function(x) sort(x))))
  
  # Pair up the rows, we'll use this later to create our groups of comments
  sorted_result$pairs <- c(paste0(sorted_result$X1, ";", sorted_result$X2))
  
  # If you have dupes, get rid of them because they won't affect our next steps
  sorted_result <- sorted_result[!duplicated(sorted_result),]
  
  # create empty data frame
  groups_of_similar <- tibble("Groups" = NA, "Number" = NA)
  
  # start index at 1
  i = 1
  
  # now sort pairs into groups
  for (comment in unique(c(sorted_result$X1, sorted_result$X2))){
    
    # take part of data frame with the comment we're currently looping through
    comment_subset <- sorted_result[grepl(pattern = comment, x = sorted_result$pairs),]
    
    # here we only want unique comment names because we're pairing up comments
    # and it's obvious doc1 and doc are similar!
    similar_comment_names <- unique(c(comment_subset$X1, comment_subset$X2))
    
    # add to data frame
    groups_of_similar[i,"Groups"] <- c(paste(sort(similar_comment_names), collapse = ";"))
    groups_of_similar[i,"Number"] <- length(similar_comment_names)
    i = i + 1
    
    # now do original dataframe too
    df[grepl(pattern = comment, x = df$doc_name),"similar_group"] <- c(paste(sort(similar_comment_names), collapse = ";"))
    df[grepl(pattern = comment, x = df$doc_name),"group_size"] <- length(similar_comment_names)

  }
  
  # one output you might want is just groups of similar comments
  comment_groups <- distinct(groups_of_similar)

  #walktrap with similarity matrix creates some odd groups
  if(missing(walktrap)){
    walktrap = F
  }
  
  if(walktrap == T){
    # https://igraph.org/r/doc/cluster_walktrap.html
    # create groups for each comment 
    df$walktrap_group <- NA
    pre_walktrap_graph <- graph_from_adjacency_matrix(as.matrix(similarities),weighted = TRUE,mode = "undirected")
    community <- cluster_walktrap( pre_walktrap_graph,weights = E(pre_walktrap_graph)$weight)
    df$walktrap_group <- community$membership
  }
  return(list(original = df, groups_only = comment_groups))
  
}

duplicate_output <- find_near_duplicates(df = small_comments, doc_text = "text",
                     doc_name = "CommentID", sim_method = "jaccard", 
                     threshold = 0.9,clstrs = 2,token_level = "word",
                     seq_length = 50, walktrap=T)

