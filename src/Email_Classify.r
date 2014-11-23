# File-Name:       Email_Classify.R       
# Code Reconstruction: Roy Zhang
# Date: 2014-11-24        
# Data Used:       Email messages contained in data/ directory, source: http://spamassassin.apache.org/publiccorpus/
# Packages Used:   tm, ggplot2

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All rights reserved.

# 装载必要用户包
library('NLP')
library('tm')
library('ggplot2')

# 设定工作路径
# YourGitHub + /GitHub/R_EmailClassify/
setwd("C:/Users/YourDream/Documents/GitHub/R_EmailClassify/")

spam.path <- "data/spam/"             #垃圾邮件1
spam2.path <- "data/spam_2/"          #垃圾邮件2
easyham.path <- "data/easy_ham/"      #容易与垃圾邮件区分的非垃圾邮件1
easyham2.path <- "data/easy_ham_2/"   #容易与垃圾邮件区分的非垃圾邮件2
hardham1.path <- "data/hard_ham/"      #很难与垃圾邮件区分的非垃圾邮件1
hardham2.path <- "data/hard_ham_2/"   #很难与垃圾邮件区分的非垃圾邮件2


###### function:get.msg ###########################
# target:邮件中抽取正文
# method:每个邮件正文和头部信息是通过一个空白行分割
# arguments:character 邮件文件路径
# return:character 邮件正文内容

get.msg <- function(path) {  
  con <- file(path, open = "rt", encoding = "latin1")  
  texts <- readLines(con)  
  breakLineNum <- which(texts == "")[1]  
    
  if( is.na(breakLineNum) || is.nan(breakLineNum) || is.infinite(breakLineNum)) {  
    cat(paste(path, breakLineNum,  "", sep=" "))  
    breakLineNum <- 0  
  }    
  msg <- texts[seq(breakLineNum, length(texts), 1)]  
  close(con)  
  return(paste(msg, collapse = "\n"))  
}  

############# funciont get.tdm  ###################
# target: 获取词频矩阵
# method: 通过tm包中携带的函数获取一系列文本的词频矩阵
# arguments: vector 文本数组
# return: matrix 词频数组
get.tdm <- function(doc.vec)
{
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE,
                  minDocFreq = 2)
  doc.corpus <- Corpus(VectorSource(doc.vec))
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

############# funciont count.word  ###################
# target: 获取词频
# method: 通过tm包中携带的函数获取文本的词频矩阵，然后进行输出
# arguments: character 邮件文件路径,单词
# return: numeric 词频
count.word <- function(path, term)
{
  msg <- get.msg(path)
  msg.corpus <- Corpus(VectorSource(msg))
  # Hard-coded TDM control
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE)
  msg.tdm <- TermDocumentMatrix(msg.corpus, control)
  word.freq <- rowSums(as.matrix(msg.tdm))
  term.freq <- word.freq[which(names(word.freq) == term)]
  #如果查找到单词则设置为相应频数，否则设为0
  return(ifelse(length(term.freq) > 0, term.freq, 0))
}

###正常邮件分类函数###  
classify.email <- function(path, training.df, prior = 0.9, c = 5e-4)
{
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  msg.match <- intersect(names(msg.freq), training.df$term)
 
  #如果一个相同的单词都没有出现，那么每个单词的概率就以默认概率的乘积
  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  else #否则出现的以出现的概率计算，没有出现的以默认概率计算，最后求乘积
  {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}

###垃圾邮件后验概率计算函数###
spamclassify.email <- function(path, training.df, prior = 0.1, c = 5e-4)
{
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  msg.match <- intersect(names(msg.freq), training.df$term)
  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  else
  {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}

# 基本函数定义完成

# 读取垃圾邮件文本
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs,
                   function(p) get.msg(file.path(spam.path, p)))


spam.tdm <- get.tdm(all.spam)
spam.matrix <- as.matrix(spam.tdm) #词文本矩阵，即这个词在某个文本中出现的次数 稀疏矩阵
spam.counts <- rowSums(spam.matrix) #每一行和，即单词在所有文本中出现的总次数
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),
                      stringsAsFactors = FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)
spam.occurrence <- sapply(1:nrow(spam.matrix), #每一行和，即出现单词的文本占所有文本的比例
                          function(i)
                          {
                            length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)
                          })
spam.density <- spam.df$frequency / sum(spam.df$frequency)

# 将density和occurrence俩个域加入到矩阵中
spam.df <- transform(spam.df,
                     density = spam.density,
                     occurrence = spam.occurrence)

# 读取正常邮件文本，过程类似。
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs[1:length(spam.docs)],
                      function(p) get.msg(file.path(easyham.path, p)))
easyham.docs <- easyham.docs[1:500]
easyham.tdm <- get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),
                         stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)
easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                            function(i)
                            {
                              length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)
                            })
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)

easyham.df <- transform(easyham.df,
                        density = easyham.density,
                        occurrence = easyham.occurrence)
# 至此，分类器已经训练完毕，我们开始使用测试集测试分类器。


### 分类器函数 ###
spam.classifier <- function(path)
{
  pr.spam <- spamclassify.email(path, spam.df)
  pr.ham <- classify.email(path, easyham.df)
  # 分类规则，贝叶斯决策，去概率较大的类作为分类。
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}

# 对所有测试集测试，来评估分类器效果。
# 读入所有测试集
easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs != "cmds")]

hardham1.docs <- dir(hardham1.path)
hardham1.docs <- hardham1.docs[which(hardham1.docs != "cmds")]

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs != "cmds")]

spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs != "cmds")]

# 对测试集进行分类
easyham2.class <- suppressWarnings(lapply(easyham2.docs,
                                   function(p)
                                   {
                                     spam.classifier(file.path(easyham2.path, p))
                                   }))
hardham1.class <- suppressWarnings(lapply(hardham1.docs,
                                   function(p)
                                   {
                                     spam.classifier(file.path(hardham1.path, p))
                                   }))								   
hardham2.class <- suppressWarnings(lapply(hardham2.docs,
                                   function(p)
                                   {
                                     spam.classifier(file.path(hardham2.path, p))
                                   }))
spam2.class <- suppressWarnings(lapply(spam2.docs,
                                function(p)
                                {
                                  spam.classifier(file.path(spam2.path, p))
                                }))

# 为数据原始情况贴上标签								
easyham2.matrix <- do.call(rbind, easyham2.class)
easyham2.final <- cbind(easyham2.matrix, "EASYHAM")

hardham1.matrix <- do.call(rbind, hardham1.class)
hardham1.final <- cbind(hardham1.matrix, "HARDHAM")

hardham2.matrix <- do.call(rbind, hardham2.class)
hardham2.final <- cbind(hardham2.matrix, "HARDHAM")

spam2.matrix <- do.call(rbind, spam2.class)
spam2.final <- cbind(spam2.matrix, "SPAM")

class.matrix <- rbind(easyham2.final,  hardham1.final, hardham2.final, spam2.final)
class.df <- data.frame(class.matrix, stringsAsFactors = FALSE)
names(class.df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")
class.df$Pr.SPAM <- as.numeric(class.df$Pr.SPAM)
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)
class.df$Class <- as.logical(as.numeric(class.df$Class))
class.df$Type <- as.factor(class.df$Type)

# 创建最终的分类结果图
class.plot <- ggplot(class.df, aes(x = log(Pr.HAM), log(Pr.SPAM))) +
    geom_point(aes(shape = Type, alpha = 0.5)) +
    stat_abline(yintercept = 0, slope = 1) +
    scale_shape_manual(values = c("EASYHAM" = 1,
                                  "HARDHAM" = 2,
                                  "SPAM" = 3),
                       name = "Email Type") +
    scale_alpha(guide = "none") +
    xlab("log[Pr(HAM)]") +
    ylab("log[Pr(SPAM)]") +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(plot = class.plot,
       filename = file.path("images", "Final_Classification.pdf"),
       height = 10,
       width = 10)
	   
# 统计函数	   
get.results <- function(bool.vector)
{
  results <- c(length(bool.vector[which(bool.vector == FALSE)]) / length(bool.vector),
               length(bool.vector[which(bool.vector == TRUE)]) / length(bool.vector))
  return(results)
}

# 输出统计结果
easyham2.col <- get.results(subset(class.df, Type == "EASYHAM")$Class)
hardham.col <- get.results(subset(class.df, Type == "HARDHAM")$Class)
spam2.col <- get.results(subset(class.df, Type == "SPAM")$Class)

class.res <- rbind(easyham2.col, hardham.col, spam2.col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)
