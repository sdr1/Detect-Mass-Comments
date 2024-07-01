#### Detect mass comments ####

library(Matrix) # to get bottom half of symmetric matrix
library(quanteda) # efficient way to store text
library(quanteda.textstats) # for fast implementation of cosine similarity
library(dplyr) # allows you to import external names

simple_group_similarity <- function(df, doc_text = "text",
                                    doc_name = "doc_name", sim_method = "cosine",
                                    threshold = 0.9){
  
  # df = small_comments
  # doc_name = "CommentID"
  # doc_text = "text"
  # sim_method = "jaccard"
  # threshold = 0.9
  
  # only select name and text columns
  df <- df %>%
    select(all_of(doc_name), all_of(doc_text)) %>%
    rename("doc_name" = 1, "comment_text" =2)
  
  # convert the data from a dataframe to a quanteda corpus
  cmt_corpus <- quanteda::corpus(x = df$comment_text,docnames = df$doc_name)
  
  # convert the corpus to a document feature matrix
  cmt_token <- quanteda::tokens(cmt_corpus)
  
  cmts_dfm <- quanteda::dfm(cmt_token)
  
  # get the similarity between texts
  similarities <- quanteda.textstats::textstat_simil(cmts_dfm, method = sim_method, min_simil = threshold)
  
  # set diagonal similarity to 0 because we know C is like C
  diag(similarities) <- 0
  
  # here's the beginning of the innovative part, decrease the dimensions of hte
  # matrix by only caring about the bottom half since the similarity between A and
  # B is the same as B and A
  similarities <- Matrix::tril(similarities)
  
  # set diagonal similarity to 0 because we know C is like C
  diag(similarities) <- 0
  
  # Need to do the double conversion from matrix to data.frame to make this work
  similarities <- as.matrix(similarities)
  
  similarities <- data.frame(similarities)
  
  colnames(similarities) <- rownames(similarities)
  
  # generate a boolean for whether there are comments with a similarity score greater than 0.9 
  # this will be by both Column and Row because the matrix is symmetric
  # now subset the data frame to only get pairs of similar comments 
  
  # use apply functions to select when x>threshold for each row and column
  # we want to save those columns
  
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

  # rename, so generic names work
  sorted_result <- sorted_result %>%
    rename(X1 = 1, X2 = 2)                                    
  
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
  
  }
  
  # one output you might want is just groups of similar comments
  comment_groups <- distinct(groups_of_similar)
  
  print(c(paste("There are", sum(comment_groups$Number), "similar comments",
                "from", length(comment_groups$Number), "groups")))
  
  return(comment_groups)
}

simple_group_similarity(df = df_test, doc_text = "words", doc_name = "nms",sim_method = "cosine")
