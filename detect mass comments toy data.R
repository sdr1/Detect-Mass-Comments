### Toy Example for Package

commentA <- "I have reviewed the proposed rule for 2014 and most strongly object to the implementation of such a dramatic change in my reimbursement without sufficient time to analyze, respond, and adapt to the draconian negative impacts that these cuts will have on access to patient care, clinical outcomes and the overall health of our patients. Our cardiology practice has absorbed almost 40% in cuts over the past three years while experience significant increases in costs, We are barely hanging on as a business. If you implement these policies we will be forced to discontinue accepting Medicare and Medicaid assignment and passing the expenses necessary to keep out doors open to our patients. This policy should be held in abeyance until we have sufficient time to fully analyze and respond."

commentB <- "Please consider addition of pharmacists as licensed health care providers that can provide chronic care management for patients especially for conditions that require multiple medications for management. It is important to consider pharmacists as an additional workforce for access, safety and improved health outcomes as you move to improve affordable and accountable care. Thank you for your consideration."

commentC <- "I have reviewed the proposed rule for 2014 and most strongly object to the implementation of such a dramatic change in physician reimbursement without sufficient time to analyze, respond, and adapt to the draconian negative impacts that these cuts will have on access to patient care, clinical outcomes and the overall health of our patients. Our cardiology practice has absorbed almost 40% in cuts over the past three years while experience significant increases in costs, We are barely hanging on as a business. If you implement these policies we will be forced to discontinue accepting Medicare and Medicaid assignment and passing the expenses necessary to keep out doors open to our patients. This policy should be held in abeyance until we have sufficient time to fully analyze and respond."

commentD <- "I have reviewed the proposed rule for 2013 and most strongly object to the implementation of such a dramatic change in physician reimbursement without sufficient time to analyze, respond, and adapt to the draconian negative impacts that these cuts will have on access to patient care, clinical outcomes and the overall health of our patients. Our cardiology practice has absorbed almost 40% in cuts over the past three years while experience significant increases in costs, We are barely hanging on as a business. If you implement these policies we will be forced to discontinue accepting Medicare and Medicaid assignment and passing the expenses necessary to keep out doors open to our patients. This policy should be held in abeyance until we have sufficient time to fully analyze and respond."

commentE <- "The purpose of this project is to better understand the techniques of persuasion that the public uses to influence regulation. In order for a regulation to become effective, an executive agency that issues the regulation must subject its proposal to public feedback. The agency must consider this public feedback in crafting a legally-binding final rule, or else an aggrieved commenter can challenge that rule in Court."

commentF <- "This comment is really similar to G but no others."

commentG <- "This comment is really similar to F but no others."

#A, B, C part of mass comment campaign, E not, F and G similar too

df_test <- data.frame(wrds = rbind(commentA, commentB, commentC, commentD, 
                          commentE, commentF, commentG), 
                 nms = c("commentA","commentB","commentC","commentD","commentE",
                           "CommentF", "CommentG"), stringsAsFactors = F)

