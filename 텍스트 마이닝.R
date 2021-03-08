## 텍스트 마이닝

## 01. 텍스트 전처리
###  - 텍스트에서 분석하는 데 불필요한 요소 제거
###  - 다루기 쉬운 형태로 만드는 과정

### 데이터 불러오기
baseball_video <- readLines("baseball_video decipher.txt", encoding="UTF-8")
head(baseball_video)

### 1. 불필요한 문자 제거 - str_replace_all()
### ex) 특수문자, 한자, 공백

txt <- "불필요한!! 문제제거. 연습용!@#"
txt

install.packages("stringr")
library(stringr)

str_replace_all(string=txt, pattern="[^가-힣]", replacement = " ") 

### [^가-힣] : 한글이 아닌 모든 문자
### 가-힣 : "가"부터 "힣"까지의 모든 한글 문자
### ^ : 반대

baseball <- baseball_video %>% 
  str_replace_all("[^가-힣]", " ")
head(baseball)

### 2. 연속된 공백 제거하기 - str_squish()
txt <- "불필요한   문제제거  연습용   "
txt
str_squish(txt)

baseball <- baseball %>% 
  str_squish()
head(baseball)

### 3. 데이터를 tibble 구조로 바꾸기 - as_tibble()
### tibble 구조 :
###  - 한 행에 한 달락이 들어있음
###  - 긴 문장은 console 창에서 보기 편할 만큼 일부만 출력
###  - 행과 열의 수를 알 수 있음
###  - 변수의 자료형을 알 수 있음

library(dplyr)
baseball <- as_tibble(baseball)
baseball

### * 지금까지 한 전처리 작업 한 번에 하기
baseball <- baseball_video %>% 
  str_replace_all("[^가-힣]", " ") %>% 
  str_squish() %>% 
  as_tibble

## 02. 토큰화

### : 텍스트를 토큰으로 나누는 작업
### 토큰(token) : 텍스트의 기본 단위 (ex: 단락, 문장, 단어, 형태소)

install.packages("tidytext")
library(tidytext)
### tidytext 패키지 : 
### 텍스트를 정돈된 데이터 형태를 유지하며 분석
### dplyr, ggplot2 패키지와 함께 사용


baseball_data <- baseball %>% 
  unnest_tokens(input = value,
                output = word,
                token = "words") 
### 띄어쓰기 기준


## 03. 단어 빈도 분석
### : 텍스트에 단어가 몇 번 사용됐는지 알아보는 분석 방법
### : 어떤 부분에서, 무엇을 강조 ?


### 1. 단어 빈도 구하기 - count()

baseball_data <- baseball_data %>% 
  count(word, sort = T)
baseball_data

### 2. 한 글자로 된 단어 제거 - filter(str_count())

baseball_data <- baseball_data %>% 
  filter(str_count(word) > 1)
baseball_data

### 3. 자주 사용된 단어 추출
top20 <- baseball_data %>% 
  head(20)
top20

library(ggplot2)
ggplot(top20, aes(x=reorder(word,n), y=n)) +
  geom_col() +
  coord_flip()

ggplot(top20, aes(x=reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +  # 막대 밖 빈도 표시
  labs(title = "야구 비디오판독 관련 기사", # 그래프 제목
       x = NULL, y = NULL) +  # 축 이름 삭제
  theme(title = element_text(size=12)) # 제목 크기


## 04. 워드 클라우드(Word Cloud) 만들기
### : 단어 빈도를 구름 모양으로 표현한 그래프
###   빈도에 따라 글자 크기와 색을 다르게 표현
###   어떤 단어가 얼마나 많이 사용됐는지 한눈에 파악

install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(baseball_data, aes(label=word, size=n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA),
               range = c(3, 30))

### 그래프 다듬기
ggplot(baseball_data,
       aes(label = word,
           size = n,
           col = n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA),
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2",   # 최소 빈도 색깔
                       high = "#004EA1")+ # 최고 빈도 색깔
  theme_minimal()  # 배경 없는 테마 적용


## 05. 그래프 폰트 바꾸기

### 1. 구글 폰트 불러오기
install.packages("showtext")
library(showtext)

font_add_google(name="Nanum Gothic", family="nanumgothic")
showtext_auto()

### 2. 그래프에 폰트 지정
ggplot(baseball_data,
       aes(label = word,
           size = n,
           col = n)) +
  geom_text_wordcloud(seed = 1234,
                      family = "nanumgothic") +
  scale_radius(limits = c(3, NA),
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2",
                       high = "#004EA1") +
  theme_minimal()


### '검은고딕' 폰트 적용
font_add_google(name = "Black Han Sans", family="blackhansans")
showtext_auto()

ggplot(baseball_data,
       aes(label = word,
           size = n,
           col= n)) +
  geom_text_wordcloud(seed = 1234,
                      family = "blackhansans") + # 폰트적용
  scale_radius(limits = c(3, NA),
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2",
                       high = "#004EA1") +
  theme_minimal()

### 3. ggplot2 패키지로 만든 그래프의 폰트 바꾸기
font_add_google(name = "Gamja Flower", family = "gamjaflower")
showtext_auto()

ggplot(top20, aes(x=reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  
  labs(title = "야구 비디오판독 관련 기사 빈도",
       x = NULL, y = NULL) +
  theme(title = element_text(size=12),text = element_text(family = "gamjaflower")) # 폰트 적용








