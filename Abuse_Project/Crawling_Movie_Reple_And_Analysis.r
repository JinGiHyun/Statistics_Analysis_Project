#Konlp 패키지를 사용하기 위해서는 rJava 패키지가 필요하다.
#자바 설치후 dyn.load 패키지를 이용해 자바를 불러오고 rJava를 설치한다.

#######중요!!!!!#######
install.packages("rJava")
source("https://install-github.me/talgalili/installr")
installr::install.java()
require(rJava)


install.packages('rvest')
install.packages('dplyr')
install.packages("stringr")

#문자를 보다 쉽게 처리하고 작업할 수 있는 패키지
#install.packages("stringr")
require(stringr)
require(rvest)
require(dplyr)

#require VS library

#require() 함수와 library() 함수의 차이는 설치되어 있지 않은 
#패키지를 불러오는 경우에 library() 함수는 오류를 발생시키지만, 
#require() 함수는 경고 메시지를 보여주는 차이가 있다. 
#require() 함수는 패키지를 불러오는 데 성공하면 TRUE, 
#실패하면 FALSE를 출력한다.


#1.영화 페이지 크롤링
movie_url='https://movie.naver.com/movie/running/current.nhn?view=list&tab=normal&order=reserve'



#2. 상위 10개 영화 크롤링

#2.1
#require(rvest)
#html=read_html(movie_url)
#html_1=html_nodes(html,'.tit')
#html_2=html_nodes(html_1,'a')
#html_3=html_attr(html_2,'href')
#View(html_3)
#위에꺼 한줄로 합치기
#require(dplyr)
n=10 #분석할 영화의 개수
html=read_html(movie_url)
movie_link=html %>% html_nodes('.tit') %>% 
  html_nodes('a') %>% 
  html_attr('href')
head(movie_link)

#2.1 각 영화의 코드 값 따오기
#require(stringr)
movie_code=str_sub(movie_link[1:n],-6,-1)
movie_code

#2.2 각 영화의 이름으로 따오기.
m_name=html %>% html_nodes('.tit') %>% html_nodes('a')
class(m_name)
##인덱싱 해주기 위해 character로 바꿔준다.
movie_name=vector()
for (i in 1:n){
  name=as.character(m_name[i])
  movie_name[i]=str_sub(name,46,-5)
}
movie_name


#3. 각 영화의 url만들기
##영화의 댓글을 보기 위해서는 각 영화의 페이지로 들어가야한다.
n=10
Each_movie=vector()
for (i in 1:n){
  Each_movie[i]=paste('https://movie.naver.com/movie/bi/mi/point.nhn?code=',
                      movie_code[i],'#tab',sep='')
}
#띄어쓰기가 있으면 URL인식이 안된다.
Each_movie=gsub('\\s','',Each_movie)
head(Each_movie)


#4. 댓글 URL만들기

##4.1 각 영화 페이지에서 댓글프레임의 URL구조를 만든다.
reple_html=read_html(Each_movie[1])
reple_frame=reple_html %>% html_nodes('.ifr_module2') %>% 
  html_nodes('iframe') %>% html_attr('src')
head(reple_frame)
#URL의 구조가 
#"https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=163608&type=after&isActualPointWriteExecute=
#false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=1"
#규칙적이다. 각 영화에서 댓글창의 다음페이지로 넘어가려면 맨끝에 페이지숫자를 바꿔주면된다.
#다른영화의 댓글로 넘어가려면 첫줄의 코드값을 위에서 만든 movie_code값으로 바꿔준다.


#4.2댓글 페이지 url따오기
#한영화당 10페이지씩
m=100 #분석할 댓글의 페이지 수 
reple_url=list()
page=vector()
for (i in 1:n){
  substr(reple_frame,42,47)=movie_code[i]
  for (j in 1:m){
    page[j]=paste('https://movie.naver.com',
                  reple_frame,'&page=',j,seq='')
  }
  #paste로 붙히면 띄어쓰기가 돼서 공백을 없애줘야한다.
  page=gsub('\\s','',page)
  reple_url[[i]]=page
}
names(reple_url)=movie_name
head(reple_url)

#5.댓글 크롤링

###example###
#랭킹1위 영화의 댓글 추출
ex_reple_html=read_html(reple_url[[1]][1])
ex_reple=ex_reple_html %>% html_nodes('.score_reple') %>% html_nodes('p')
head(ex_reple)
#태그’p’로 들어갔었는데 2번째 줄에서 댓글이 잘린다.

#그래서 태그’a’에 있는 속성onclick’으로 들어갔다.
ex_reple=ex_reple_html %>% html_nodes('.score_reple') %>% html_nodes('a') %>% html_attr('onclick')
head(ex_reple)



#댓글이 있는 부분인 짝수행에서 댓글만 인덱싱 해준다.
##한글아닌것 전부 제거 str_replace_all를 이용해서 제거하려했는데
##띄어쓰기도 사라지고 안지워지는 문자도 있어서 인덱싱을 해준다.
##reple_word=str_replace_all(m_reple_1[j],'[A-z]','')
##reple_word=str_replace_all(reple_word,'[0-9]','')
##reple_word=str_replace_all(reple_word,'[^[:alnum:]]','')
##reple[j]=reple_word

ex_reple_1=ex_reple[seq(2,length(ex_reple),2)]
ex_reple_2=str_sub(ex_reple_1,135,-52)
head(ex_reple_2,30)



#5.1댓글 추출
movie_reple=list()
movie_reple_all=list()
for (i in 1:n){
  for (k in 1:m){
    reple_html_2=read_html(reple_url[[i]][k])
    m_reple=reple_html_2 %>% html_nodes('.score_reple') %>% html_nodes('a') %>% html_attr('onclick')
    m_reple_1=m_reple[seq(2,length(m_reple),2)]
    movie_reple[[k]]=str_sub(m_reple_1,135,-52)
  }
  movie_reple_all[[i]]=unlist(movie_reple)
}


#각 리스트 이름을 영화 제목으로 바꾼다.
movie_reple_all_1=movie_reple_all
names(movie_reple_all_1)=movie_name
head(movie_reple_all_1[[1]],30)
str(movie_reple_all)

#시각화
install.packages('KoNLP')
install.packages('wordcloud')
install.packages('RColorBrewer')

library(RColorBrewer)
library(KoNLP)
useNIADic()


word_1=movie_reple_all[1]
#댓글로부터 명사들만 추출
#USE.NAMES=T 이름 속성 반환 / 
#USE.NAMES=F 이름 속성 없이 반환
word_2=sapply(word_1,extractNoun,USE.NAMES=F)
word_2
head(unlist(word_2),30)

#벡터 형태로 저장
word_3=unlist(word_2)
length(word_3)
#2글자 이상 단어만 추출
word_3 = Filter(function(x){nchar(x)>=2},word_3)
length(word_3)
head(word_3)
heatmap(word_3)


#텍스트 파일로 저장; 워드클라우드를 꾸미기 위해(tagxedo)
write(word_3,"word_1.txt")

wordcount=table(word_3)
head(sort(wordcount, decreasing=T),20)


#글자 색 지정
palete <- brewer.pal(9,"Set1") 

library(wordcloud)
#워드클라우드작성
#freq : 빈도는 언급된 단어수
#scale : 폰트 사이즈 조정
#rot.per : 수직 텍스트의 비율 조정
#min.freq : 최소 몇번의 언급된 단어만 추출
#random.order : 순서 랜덤
#random.color : 컬러 랜덤
wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.25),rot.per=0.25,min.freq=5,random.order=F,random.color=T,colors=palete)


#######################################################
#키워드 분석
names(movie_reple_all_1[1])
for (i in 1:n){
  word_1=movie_reple_all_1[[i]]
  word_2=str_replace_all(word_1,'[A-zㄱ-ㅎ0-9]','')
  word_3=str_replace_all(word_2,'영화','')
  word_4=sapply(word_3,extractNoun,USE.NAMES=F)
  word_5=unlist(word_4)
  word_6=Filter(function(x){nchar(x)>=2},word_5)
  x=paste(names(movie_reple_all_1[i]),'.txt',seq='')
  x=gsub('\\s','',x)
  write(unlist(word_6),x)
}
write(word_6,'ssss.txt',encoding='UTF-8')

palete <- brewer.pal(9,"Set1")
for (i in 1:10){
  word_1=movie_reple_all_1[[i]]
  word_2=str_replace_all(word_1,'[A-zㄱ-ㅎ0-9]','')
  word_3=str_replace_all(word_2,'영화','')
  word_4=sapply(word_3,extractNoun,USE.NAMES=F)
  word_5=unlist(word_4)
  word_6=Filter(function(x){nchar(x)>=2},word_5)
  write(unlist(word_6),"word_1.txt")
  word_7=read.table("word_1.txt")
  wordcount=table(word_7)
  wordcloud(names(wordcount),freq=wordcount,  scale=c(4,1),
            rot.per=0.25, min.freq=4,random.order=F,
            random.color=T,colors=palete)
}