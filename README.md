# Detect Mass Comments

This is a repository for my R script to detect mass comments.  

Mass comments are a common feature of high salience rules.  Detecting them is difficult for two reasons: (1) it is difficult to set a correct threshold of similarity to distinguish mass comments and (2) there can be millions of comments, so the algorithm must be computationally efficient or else it isn't useful except for rules with only a few thousand comments.

This respository will contain two files, one to create a similarity matrix (quick/easy) and one that processes the similarity matrix and creates communities out of comments with alike similarity scores.

For bugs/suggestions email me at steven_rashin@radcliffe.harverd.edu or srashin@gmail.com
