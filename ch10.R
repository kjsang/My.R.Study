install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages('rJava')
install.packages("memoise")

library(RColorBrewer)
library(wordcloud)
library(rJava)
library(memoise)
library(KoNLP)

install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))


# KoNLP 설치방법: 첫 번쨰 방법

install.packages('devtools')
devtools::install_github('haven-jeon/KoNLP') # 3 클릭

rm(list)

# 워드클라우드 만들기~
getwd()
useSejongDic()
pal2 <- brewer.pal(8, "Dark2")
text <- readLines("취임사_노무현.txt", encoding = "EUC-KR")
# 한줄씩 읽기 위해
# UTF-8 깨지면 EUC-KR 인코딩
noun <- sapply(text, extractNoun, USE.NAMES = F) 
noun

# 리스트형태를 해체할 때
noun2 <- unlist(noun)
noun2
wordcount <- table(noun2)
temp <- sort(wordcount, decreasing = T)[1:10]
temp
temp <- temp[-1]

barplot(temp, las = 2,
        names.arg = names(temp),
        col = "lightblue")

wordcloud(names(wordcount),
          freq = wordcount,
          scale = c(30,0,30),
          min.freq = 3, # 최소빈도
          random.order = F,  # 빈도수에 따른 장소를 중앙에 배치
          rot.per = .1, # 90도 회전 단어 비율
          colors=pal2, 
          )



rm(list=ls())

# 사전 설정
install.packages("KoNLP")
library(KoNLP)
useSystemDic()
useSejongDic()
useNIADic()

# 파일 불러오기
word_data <- readLines("취임사_노무현.txt", encoding = "EUC-KR")
word_data

# 명사만 추출
word_data2 <- extractNoun(word_data)

# 사용빈도 확인
undata <- unlist(word_data2)
word_table <- table(undata)

undata2 <- Filter(function(x){ nchar(x) > 1 }, undata)
word_table2 <- table(undata2) # undata2의 빈도 확인 후 word_table2 변수에 할당당
word_table2

sort(word_table2, decreasing = T)

library(RColorBrewer)
display.brewer.all()  # 워드클라우드에 쓸 색상 리스트

palette <- brewer.pal(9,"Spectral")

install.packages("wordcloud")
library(wordcloud)
wordcloud(names(word_table2),
          freq = word_table2,
          scale = c(5,0.5),
          rot.per = 0.1,        # 글씨 방향의 비율 (0 : 가로만/ 1: 세로만/ 0.x : 비율)
          min.freq = 4,       # 나타내는 최소 언급 값
          random.color = T,   # 글자 색 임의로 지정
          random.order = F,   # 빈도수에 따른 장소를 중앙에 배치
          colors = palette,   # 글자 색
          family = "맑은 고딕")

# 또다른 워드클라우드 패키지가 있다.

install.packages("wordcloud2")
library(wordcloud2)

wordcloud2(word_table2, 
           color = "random-light", 
           backgroundColor = "black",
           shape = "pentagon") # circle(기본), diamond, triangle, triangle-forward, pentagon
?wordcloud2

# 네이버 데이터랩

letterCloud(data=word_table2,word='R',wordSize=3,fontFamily='맑은고딕')

