install.packages("N2H4")
library(N2H4)
library(dplyr)
tar <- "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313"
getAllComment(tar) %>%
	select(userName, contents)

# install.packages("tidytext")
library(tidytext)
tar <- "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313"
getAllComment(tar) %>%
  select(userName, contents) %>%
  unnest_tokens(ws, contents, "words")

# install.packages("rJava")
library(rJava)

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--np-multiarch"))
library(KoNLP)
SimplePos09("롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.")

install.packages("RmecabKo")
RmecabKo::install_mecab("c:/Rlib/mecab")
library(RmecabKo)

# 하나의 뉴스 데이터를 가져와서
getContent("https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313") %>% 
  select(body) %>%
# 본문 컬럼을 word 단위로 쪼갠 결과물을 word라는 컬럼으로 출력
  unnest_tokens(input = body,
                output = word)

# unnest_tokens(
#   tbl = 텍스트 데이터,
#   input = 목표 텍스트 열,
#   output = 결과열의 이름,
#   token = "words", <- 여기에 형태소 분석 함수를 적용
#   ...
# )

library(KoNLP)
# 뉴스 댓글을 가져와서
getAllComment(
  "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313"
) %>%
  # 사용자 아이디와 댓글 컬럼만 선택하고
  select(userName, contents) %>%
  # 댓글 컬럼을 형태소 단위로 쪼개
  # pos라는 컬럼으로 출력
  unnest_tokens(pos, contents,
                token = SimplePos09) %>% # 쌍따옴표 쓰지 말자
  # 사용자 별로 그룹 지어서
  group_by(userName) %>%
  # pos 결과물의 순서 보장을 위해 순서 값을 추가
  mutate(pos_order = 1:n()) %>% 
  ungroup -> pos_res

# # A tibble: 4,026 x 3
# # Groups:   userName [224]
# userName pos         pos_order
# <chr>    <chr>           <int>
#   1 mooz**** 조/n                1
# 2 mooz**** :/s                 2
# 3 mooz**** 대따/n              3
# 4 mooz**** 국/n                4
# 5 mooz**** :/s                 5
# 6 mooz**** 밀/p+ㄴ들/e         6
# 7 mooz**** 한테/n              7
# 8 mooz**** 걸렸/n              8
# 9 mooz**** 다/m                9
# 10 cons**** 그래/i+,/s          1
# # ... with 4,016 more rows

library(stringr)
pos_res %>%
  # 우선 `filter()` 와 `str_detect()` 함수를
  # 활용하여 명사(n)만 추출
  filter(str_detect(pos, "/n")) %>%
  # 형태소 정보를 제거
  mutate(pos_done = str_remove(pos, "/.*$")) ->
  n_done
n_done

# 쌍따옴표 쓰면 안 된다.

pos_res %>%
  filter(
    str_detect(pos, "/p")
  ) %>%
  mutate(
    pos_done =
      str_replace_all(pos,
                      "/.*$", "다")
  ) ->
  p_done
bind_rows(n_done, p_done) %>%
  arrange(pos_order) %>%
  filter(nchar(pos_done) > 1) %>%
  select(userName, pos_done) ->
  pos_done
pos_done

getAllComment(
  "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313"
) %>%
  select(userName, contents) %>% 
  mutate(contents = str_remove_all(contents, " ")) 
# all 이 없으면 첫 번째 것만 없어진다.


# install.packages("remotes")
remotes::install_github("mrchypark/multilinguer", force = T)
library(multilinguer)
multilinguer::install_conda()
install.packages("reticulate")
reticulate::install_miniconda()

remotes::install_github("haven-jeon/KoSpacing")
library(KoSpacing)
set_env()
spacing("롯데마트가판매하고있는흑마늘양념치킨이논란이되고있다.")
