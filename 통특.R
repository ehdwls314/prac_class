
index_data=fread("C:/Users/Dongjin/Downloads/index")
getwd("C:/Users/Dongjin/Downloads/index.csv")
index <- fread("index.csv", encoding="UTF-8")
head(index)

#코로나 발생 이후의 데이터 추출

index = subset(index,period>=202001 & period<=202005)
index

#
index$cgii = as.numeric(index$cgi>100)
as.numeric(index$cgi>100)
index$cgii
length(which((index$catl=='건강/의료용품')&(index$cgii==1)))/length(which(index$catl=='건강/의료용품'))
length(which((index$catl=='식품')&(index$cgii==1)))/length(which(index$catl=='식품'))
length(which((index$catl=='일용품')&(index$cgii==1)))/length(which(index$catl=='일용품'))
length(which((index$catl=='화장품')&(index$cgii==1)))/length(which(index$catl=='화장품'))


df =  data.frame(catl=c('건강/의료용품',"식품","일용품","화장품"), 
                 per=c(0.368285,0.313996,0.171658,0.159830))
df

ggplot(data=df, aes(x=catl, y=per,fill=catl)) +
  ylim(0,1) +
  geom_bar(stat="identity")  +
  geom_text(aes(label=per), color = "white", vjust=1.6, size=3.5)


###########################################################################

install.packages("tidyverse")

# 데이터 로딩
library(dplyr)
library(tidyverse)
library(glue)
library(data.table)

#카드 데이터 불러오기
setwd("C:/Users/Dongjin/Downloads/KT_data_20200717")
install.packages('bit64')
card <- fread("card_20200717.csv", encoding="UTF-8")


#카드 데이터 정제
head(card)
card1<-filter(card,selng_cascnt<0)#음수값 있는지 확인
card1
card1<-filter(card,salamt<0)#음수값 있는지 확인
card1

# selng_cascnt,salamt 두 변수 모두 음수값과 텍스트 데이터를 포함하기 때문에 텍스트 데이터 제외 및 음수값 처리가 필요함.

#텍스트 데이터 제거
card <- filter(card,!str_detect(selng_cascnt, regex("[ㄱ-ㅎ가-힣]")))#텍스트 데이터 없애기
card <- filter(card,!str_detect(salamt, regex("[ㄱ-ㅎ가-힣]")))#텍스트 데이터 없애기

#숫자 데이터 숫자형식으로 지정 및 음수값 절대값을 통해 양수 값으로 처리
card$selng_cascnt<-as.numeric(card$selng_cascnt)#숫자 데이터로 바꾸기
card$salamt<-as.numeric(card$salamt)#숫자 데이터로 바꾸기
card$selng_cascnt<-abs(card$selng_cascnt)#음수값 처리하기 위해 절대값 처리해줌
card$salamt<-abs(card$salamt)#음수값 처리하기 위해 절대값 처리해줌

#card의 mrhst_industry_cl_code는 가맹점 업종코드 
V2 <- substr(card$mrhst_induty_cl_code,1,1)
V2
card<-cbind(card, V2)#industry코드의 앞부분은 카테고리를 뜻하므로 나눔
#카테고리형을 character형으로 변환
card$V2<-gsub("1","여행&교통수단",card$V2)
card$V2<-gsub("2",'스포츠&문화&여가',card$V2)
card$V2<-gsub("3",'생활용품&주유',card$V2)
card$V2<-gsub("4",'패션&쇼핑',card$V2)
card$V2<-gsub("5",'교육&사무',card$V2)
card$V2<-gsub("6",'차량&보험',card$V2)
card$V2<-gsub("7",'의료&미용',card$V2)
card$V2<-gsub("8",'식품&외식',card$V2)
card$V2<-gsub("9",'기타',card$V2)

card<-cbind(card,V2)#위에 이름형으로 바꿔준것 변수화 하고, 열에 추가해준것임
card<-cbind(card, substr(card$receipt_dttm,5,6))#데이터를 월별로 보고자, 20200225라면 02(월)만 빼서 변수화 시켜주고, 마찬가지로 열에 추가
names(card) <- c('date', 'ad_code', 'ad_nm', 'ind_code', 'ind_nm', 'sal_cnt', 'sal_amt', 'cate_name','cate_code','month') #변수 이름 헷갈려서 쉽게 바꿈


install.packages("devtools")
install.packages("lubridate")
library(lubridate)

#카테고리별 데이터 나누기

for (i in 1:9){assign(paste0("card",i),filter(card, cate_code == i))}
card$sal_cnt <- as.numeric(card$sal_cnt)
card$sal_amt <- as.numeric(card$sal_amt)

#카테고리별 월별 결제건수 계산
for (x in 1:9){
  A<-filter(card, cate_code == x)
  assign(paste0("category1",x),aggregate(sal_cnt~month,A,sum))
}

#카테고리별 월별 결재금액 계산
for (x in 1:9){
  A<-filter(card, cate_code == x)
  assign(paste0("category2",x),aggregate(sal_amt~month,A,sum))
}

# 날짜 컬럼 날짜 형식으로 변환
card$date <- ymd(card$date)

#card table 만들기
view_card <- card[,-10]
colnames(view_card) <- c("날짜", "행정동코드","행정동명","가맹점업종코드","가맹점업종명","매출발생건수","매출발생금액","가맹점 카테고리","카테고리이름")

install.packages("plotly")
library(plotly)
card_monthly <- aggregate(sal_amt ~ cate_name + month, card, sum)
ggplotly(ggplot(card_monthly, aes(x=month, y=sal_amt, color=cate_name)) + geom_point()+ theme(legend.direction="horizontal", legend.position="bottom",legend.text=element_text(size=12))) %>% plotly::config(displayModeBar=F)



sale_amount_data <- aggregate(sal_amt ~ cate_name, card, sum)
ggplot(sale_amount_data, aes(reorder(cate_name, sal_amt), y=sal_amt, fill=factor(cate_name)))+geom_col() + coord_flip() +
  labs(fill="category")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_blank()
  )


ggplot(sale_count_data, aes(x=cate_name, y=sal_cnt, fill=factor(cate_name)))+
  geom_col(width=1, color="white")+
  labs(fill="category")+
  coord_polar()+
  labs(x="",y="")+
  theme_minimal()+
  theme(
    legend.text = element_text(size=12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face="bold", size=12),
    plot.title = element_text(size=12, face="bold")
  )



##########################################################


card$receipt_dttm <- ymd(card$receipt_dttm)
card$adstrd_code <- card$adstrd_code %/% 100
card <- merge(card, ad.master, by = 'adstrd_code') %>% mutate(month = month(receipt_dttm)) %>% filter(month != 6)

card <- card%>% mutate(month = month(receipt_dttm)) %>% filter(month != 6)
card <- card %>% filter(month(receipt_dttm) != 6)
head(card)
tail(card)

# 날짜별 매출건수, 매출액 변화
card.date <- group_by(card, receipt_dttm) %>% summarize(day_selng_cascnt = mean(selng_cascnt),
                                                        day_salamt = mean(salamt))
head(card.date)

# 날짜별 평균 매출 건수와 매출액 변화 

p14.1 <- ggplot(card.date) +
  geom_line(mapping = aes(x = receipt_dttm, y = day_selng_cascnt), color = '#2255EE99', lwd = 1.2) +
  ggtitle("The change of average selling count") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

p14.2 <- ggplot(card.date) +
  geom_line(mapping = aes(x = receipt_dttm, y = day_salamt), color = '#EE552299', lwd = 1.2) +
  ggtitle("The change of average selling amount") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

p14.1
p14.2

# 매출건수와 매출액이 전반적으로 1월 말 ~ 2월 초를 기준으로 감소한 것을 알 수 있음





