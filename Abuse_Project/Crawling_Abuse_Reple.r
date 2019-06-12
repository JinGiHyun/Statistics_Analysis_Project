require(stringr)
require(rvest)
require(dplyr)

url='https://dcnewsj.joins.com/article/23473548?cloc=dcnewsj|list|rankingnews'
html=read_html(url)

#댓글 프레임
reple_frame=html %>%
  html_nodes('.comment_area') %>%
  html_nodes('iframe') %>%
  html_attr('src')

# 각 기사 코드 추출
code=html %>%
  html_nodes('.text_center') %>%
  html_nodes('a') %>%
  html_attr('href')
code_all=str_sub(code, 10, -1)

#댓글 프레임 url 만들기
reple_url=str_sub(reple_frame, 1, -9)

# 각 기사 댓글 프레임 url 만들기
reple_url_all=vector()
for (i in 1:length(code_all)){
  reple_url_all[i]=paste0(reple_url,code_all[i])
}
reple_url_all[19]=reple_frame

#댓글 추출
abuse=list()
for (i in 1:length(reple_url_all)){
  html2=read_html(reple_url_all[i])
  abuse[[i]]=html2 %>%
    html_nodes('.gallery_re_list') %>%
    html_nodes('.reply') %>%
    html_text()
}
abuse=unlist(abuse)

# 인덱싱
num=c('0','1','2','3','4','5',
      '6','7','8','9')
a=str_sub(abuse,12,-8)
for(i in 1:length(a)){
  if (str_sub(a[i],-1) == '*'){
    a[i]=str_sub(a[i], 1, -11)
    if (str_sub(a[i],-1) %in% num){
      a[i]=str_sub(a[i],1,-2)
    }
  }
}
abuse_reple=a
View(abuse_reple)
class(abuse_reple)
write.csv(abuse_reple, 'Abuse_reple.csv', fileEncoding='UTF-8')
