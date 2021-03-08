## ※  띄어쓰기 기준 토크화의 문제 :
### 의미를 지니지 않는 서술어가 가장 많이 추출

## 01. 형태소 분석 (Morphological Analysis)
### - 문장에서 형태소를 추출해 명사, 동사, 형용사 등 품사로 분류하는 작업
### - 특히 명사를 보고 문장 내용 파악
### - 형태소 : 의미를 가진 가장 작은 말의 단위
###            더 나누면 뜻이 없는 문자가 됨

install.packages("cachem")
# 자바 , rJava 패키지 설치
install.packages("multilinguer")
library(multilinguer)
install_jdk()

# KoNLP 의존성 패키지 설치
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")

# KoNLP 패키지 설치
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))
library(KoNLP)
install.packages("ps")

# 형태소 사전 설정
useNIADic()
library(dplyr)

### 데이터 불러오기

baseball_data <- readLines("baseball_video decipher.txt", encoding = "UTF-8")

### 기본 전처리

library(stringr)
library(textclean)

baseball <- baseball_data %>% 
  str_replace_all("[^가-힣]", " ") %>%  # 한글만 남기기
  str_squish() %>% # 중복 공백 제거
  as_tibble() # tibble로 변환
baseball


### 명사 기준 토큰화
library(tidytext)
word_noun <- baseball %>% 
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
word_noun


## 02.명사 빈도 분석

word_noun <- word_noun %>% 
  count(word, sort = T) %>%  # 내림차순 정렬
  filter(str_count(word) > 1) # 두 글자 이상만 남기기
word_noun # 기사가 166개의 명사로 구성됨

top20 <- word_noun %>% 
  head(20)

library(ggplot2)

ggplot(top20, aes(x = reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
# 명사로 되어있기에 연설문의 내용을 이해하기 쉬움


### 워드 클라우드 만들기
library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()

library(ggwordcloud)
ggplot(word_noun, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(3, NA),
               range = c(3, 15)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
  theme_minimal()

## 03. 특정 단어가 사용된 문장 살펴보기
### 고빈도 단어 사용된 문장 직접 읽기
### 글쓴이가 어떤 의미로 단어를 사용했는지 이해 가능

sentences_baseball <- baseball %>% 
  str_squish() %>% 
  as_tibble() %>% 
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")
sentences_baseball

# 이때 문장으로 토큰화를 하기 때문에 문장의 기준점이 되는 마침표가 있으므로, 특수 문자 제거 x


### 특정 단어가 사용된 문장 추출
sentences_baseball %>% 
  filter(str_detect(sentence, "논란"))

