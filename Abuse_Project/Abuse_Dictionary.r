###########################################################################
#1.크롤링
yok_url='https://namu.wiki/w/%EC%9A%95%EC%84%A4/%ED%95%9C%EA%B5%AD%EC%96%B4'

require(stringr)
require(rvest)
require(dplyr)
require(KoNLP)


#text 가져오기
html=read_html(yok_url)
yok_crawling=html %>%
  html_nodes('.wiki-heading-content') %>% 
  html_nodes('.wiki-list') %>% 
  html_nodes('.wiki-paragraph') %>% 
  html_text()


###########################################################################
#2.사전만들기

#단어와 설명 나누기
df1=data.frame(word=NA,description=NA)
for(i in 1:length(yok_crawling)){
  df1[i,1]=str_split(yok_crawling,":")[[i]][1]
  df1[i,2]=str_split(yok_crawling,":")[[i]][2]
}


# ","로 되어있는 단어들 나누기
word_split=list()
descript_split=list()
for(i in 1:nrow(df1)){
  word_split[[i]]=if(length(unlist(str_split(df1[i,"word"],","))) >=1){unlist(str_split(df1[i,"word"],","))}
  word_split[[i]]=gsub('\\s','',word_split[[i]])
  descript_split[[i]]=rep(df1[i,2],length(word_split[[i]]))
}
df2=data.frame(word=unlist(word_split),description=unlist(descript_split),stringsAsFactors = F)
View(df2)


#욕설을 제외한 부가설명 지워주기
for(i in 1:nrow(df2)){
  df2[i,1]=str_split(df2[,1],'\\(')[[i]][1]
  df2[i,1]=str_split(df2[,1],'\\[')[[i]][1]
  df2[i,1]=str_split(df2[,1],'\'')[[i]][1]
  df2[i,1]=str_split(df2[,1],'=')[[i]][1]
  df2[i,1]=str_split(df2[,1],'\\.')[[i]][1]
  df2[i,1]=gsub('-','',df2[i,1])
  df2[i,1]=gsub('\\)','',df2[i,1])
}
View(df2)
write.csv(df2[1:2], 'Abuse_Dic.csv', fileEncoding='UTF-8')
#2.1초성 사전 만들기
#욕사전 초성으로 바꾸기
for(i in 1:nrow(df2)){
  initial=vector()
  jamos=convertHangulStringToJamos(df2[i,1])
  for(j in 1:length(jamos)){
    jamos2=convertHangulStringToJamos(jamos[j])
    initial[j]=jamos2[1]
  }
  initial_sum=paste(initial,collapse='')
  df2[i,'initial']=initial_sum
}
head(df2$initial)

#2.2자음,모음으로 분리된 사전 만들기
df_jamo=list()
for (i in 1:nrow(df2)){
  hangul=convertHangulStringToJamos(df2[i,1])
  df_jamo[[i]]=hangul
}
head(df_jamo)




###########################################################################
#3.욕판별 하는 함수 만들기
#3.1숫자, 영어 특수문자 제거
yok_num=function(word){
  word=str_replace_all(word,'[A-z0-9]','')
  if (word %in% df2[,1]){
    word2=df2[which(df2[,1]==word),2]
    return(word2)
  }
}
yok_num('시1발')

#3.2 자음으로 들어온 단어
yok_jaum=function(word){
  word=str_replace_all(word,'[A-z0-9]','')
  if(word %in% df2[,3]){
    word2=df2[which(df2[,3] == word),2]
    return(word2)
  }
}
yok_jaum('ㅅㅂ')

#3.3 이상한 단어 바꿔주기
yok_strange=function(word){
  word=str_replace_all(word,'[A-z0-9]','')
  word2=convertHangulStringToJamos(word)
  word3=Filter(function(x){nchar(x)>=2},word2)
  for (i in 1:length(df_jamo)){
    word4=word3[word3 %in% df_jamo[[i]]]
    if(length(word4) == length(df_jamo[[i]])){
      word5=paste(word4,collapse='')
      word6=HangulAutomata(word5)
      return(df2[which(df2[,1]==word6),2])
    }
  }
}
yok_strange('시ㅣ이이이ㅣㅣㅣ1발')


###########################################################################
#4.욕변환
yok_convert=function(word){
  word=str_replace_all(word,'[A-z0-9]','')
  if (word %in% df2[,1]){
    return(df2[which(df2[,1]==word),2])
  }
  else{
    word2=convertHangulStringToJamos(word)
    word3=Filter(function(x){nchar(x)>=2},word2)
    for (i in 1:length(df_jamo)){
      word4=word3[word3 %in% df_jamo[[i]]]
      if(length(word4) == length(df_jamo[[i]])){
        word5=paste(word4,collapse='')
        word6=HangulAutomata(word5)
        return(df2[which(df2[,1]==word6),2])
      }
    }
  }
}
yok_convert('시이이이이이이ㅣ1발')

