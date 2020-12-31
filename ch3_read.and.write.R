#csv파일은 칼럼을 콘마로 구분한 포멧이다.
#read.csv 함수로 불러올 수 있다.
#write.csv 함수로 저장할 수 있다.

#write.csv(저장할 변수, "파일이름", 
#             row.names=F(행의 이름을 붙이지 마라), 
#             quote=F(문자열을 저장할 경우 문자열 공백/따옴표 여부))


getwd()
setwd(getwd())
mydata <- read.csv("test.csv")
myRow1 <- mydata[2,]
myRow2 <- mydata[,3]

mynew <- mydata[,c(2,3)]
write.csv(mynew,
          "kid_new.csv",
          row.names=F,
          quote=F)
#파일탐색기로 찾아서 읽어오는 방법이 있다.
#mydata2 <- read.csv(file.choose(파일이 있는 폴더 지정),
#                    header=T) #헤더는 칼럼네임을 함께 가져올지 여부
mydata2 <- read.csv(file.choose(파일이 있는 폴더 지정),
                    header=T)

#한글이 포함된 데이터를 깨짐없이 불러오고 싶을 때,
#한글에도 여러 종류가 있다. 인코딩 방법의 대표적 방법이 UTF-8, euc-kr이다.
#euc-kr의 경우 읽지 못하기 때문에 인코딩옵션을 바꿔줘야 한다.
#csv 저장할 때 파일포멧형태를 UTF-8로 저장하면 된다.


gradecsv <- read.csv("0202.grade.csv",
                     header=TRUE, # T만 적어도 됨. FALSE 는 F만 적어도 됨
                     sep = ",", # 기본값이 R에서는 ,로 인식하기 때문에 빼주어도 됨
                     na.strings = ".")

str(gradecsv)

gradecsv$csex <- factor(gradecsv$csex,
                        levels = c(1,2),
                        labels = c("남자","여자")) 
# 변수를 수정하는 것(업데이트) : 똑같은 변수를 왼쪽과 오른쪽에 입력

str(gradecsv)

# 데이터 내보내기

write.csv(gradecsv,
          file = "gradecsv1.csv",
          row.names = F,
          na="")

# R 데이터로 저장하기 

save(gradecsv, file="grade.Rdata")

load(file ="grade.Rdata")


