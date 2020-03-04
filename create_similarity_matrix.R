#### Detect mass comments ####
library(FeatureHashing); library(Matrix); library(xgboost)
library(quanteda)

commentA <- "I have reviewed the proposed rule for 2014 and most strongly object to the implementation of such a dramatic change in my reimbursement without sufficient time to analyze, respond, and adapt to the draconian negative impacts that these cuts will have on access to patient care, clinical outcomes and the overall health of our patients. Our cardiology practice has absorbed almost 40% in cuts over the past three years while experience significant increases in costs, We are barely hanging on as a business. If you implement these policies we will be forced to discontinue accepting Medicare and Medicaid assignment and passing the expenses necessary to keep out doors open to our patients. This policy should be held in abeyance until we have sufficient time to fully analyze and respond."

commentB <- "Please consider addition of pharmacists as licensed health care providers that can provide chronic care management for patients especially for conditions that require multiple medications for management. It is important to consider pharmacists as an additional workforce for access, safety and improved health outcomes as you move to improve affordable and accountable care. Thank you for your consideration."

commentC <- "I have reviewed the proposed rule for 2014 and most strongly object to the implementation of such a dramatic change in physician reimbursement without sufficient time to analyze, respond, and adapt to the draconian negative impacts that these cuts will have on access to patient care, clinical outcomes and the overall health of our patients. Our cardiology practice has absorbed almost 40% in cuts over the past three years while experience significant increases in costs, We are barely hanging on as a business. If you implement these policies we will be forced to discontinue accepting Medicare and Medicaid assignment and passing the expenses necessary to keep out doors open to our patients. This policy should be held in abeyance until we have sufficient time to fully analyze and respond."

commentD <- "I have reviewed the proposed rule for 2013 and most strongly object to the implementation of such a dramatic change in physician reimbursement without sufficient time to analyze, respond, and adapt to the draconian negative impacts that these cuts will have on access to patient care, clinical outcomes and the overall health of our patients. Our cardiology practice has absorbed almost 40% in cuts over the past three years while experience significant increases in costs, We are barely hanging on as a business. If you implement these policies we will be forced to discontinue accepting Medicare and Medicaid assignment and passing the expenses necessary to keep out doors open to our patients. This policy should be held in abeyance until we have sufficient time to fully analyze and respond."

commentE <- "The purpose of this project is to better understand the techniques of persuasion that the public uses to influence regulation. In order for a regulation to become effective, an executive agency that issues the regulation must subject its proposal to public feedback. The agency must consider this public feedback in crafting a legally-binding final rule, or else an aggrieved commenter can challenge that rule in Court."

#A and B part of mass comment campaign, C not

df <- as.data.frame(rbind(commentA, commentB, commentC, commentD, commentE), stringsAsFactors = F)

cmt_corpus <- corpus(df$V1,docnames = c("commentA","commentB","commentC","commentD","commentE"))

cmts <- dfm(cmt_corpus)

similarities <- textstat_simil(cmts, method = c("cosine"), upper = F,diag = F)

similarities <- as.matrix(similarities)

diag(similarities) <- 0

similarities <- data.frame(similarities, stringsAsFactors = F)

### generate a boolean for whether there are comments with a similarity score greater than 0.9 
### this will be by both Column and Row because the matrix is symmetric

any_similar_boolean <- sapply(X = similarities,FUN = function(x){any(x>0.9)})

#now subset the data frame to only get pairs of similar comments 

similarities <- similarities[any_similar_boolean,any_similar_boolean]
