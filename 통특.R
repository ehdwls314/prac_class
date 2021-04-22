
index_data=fread("C:/Users/Dongjin/Downloads/index")
getwd("C:/Users/Dongjin/Downloads/index.csv")
index <- fread("index.csv", encoding="UTF-8")
head(index)

#ÄÚ·Î³ª ¹ß»ı ÀÌÈÄÀÇ µ¥ÀÌÅÍ ÃßÃâ

index = subset(index,period>=202001 & period<=202005)
index

#
index$cgii = as.numeric(index$cgi>100)
as.numeric(index$cgi>100)
index$cgii
length(which((index$catl=='°Ç°­/ÀÇ·á¿ëÇ°')&(index$cgii==1)))/length(which(index$catl=='°Ç°­/ÀÇ·á¿ëÇ°'))
length(which((index$catl=='½ÄÇ°')&(index$cgii==1)))/length(which(index$catl=='½ÄÇ°'))
length(which((index$catl=='ÀÏ¿ëÇ°')&(index$cgii==1)))/length(which(index$catl=='ÀÏ¿ëÇ°'))
length(which((index$catl=='È­ÀåÇ°')&(index$cgii==1)))/length(which(index$catl=='È­ÀåÇ°'))


df =  data.frame(catl=c('°Ç°­/ÀÇ·á¿ëÇ°',"½ÄÇ°","ÀÏ¿ëÇ°","È­ÀåÇ°"), 
                 per=c(0.368285,0.313996,0.171658,0.159830))
df

ggplot(data=df, aes(x=catl, y=per,fill=catl)) +
  ylim(0,1) +
  geom_bar(stat="identity")  +
  geom_text(aes(label=per), color = "white", vjust=1.6, size=3.5)


###########################################################################

install.packages("tidyverse")

# µ¥ÀÌÅÍ ·Îµù
library(dplyr)
library(tidyverse)
library(glue)
library(data.table)

#Ä«µå µ¥ÀÌÅÍ ºÒ·¯¿À±â
setwd("C:/Users/Dongjin/Downloads/KT_data_20200717")
install.packages('bit64')
card <- fread("card_20200717.csv", encoding="UTF-8")


#Ä«µå µ¥ÀÌÅÍ Á¤Á¦
head(card)
card1<-filter(card,selng_cascnt<0)#À½¼ö°ª ÀÖ´ÂÁö È®ÀÎ
card1
card1<-filter(card,salamt<0)#À½¼ö°ª ÀÖ´ÂÁö È®ÀÎ
card1

# selng_cascnt,salamt µÎ º¯¼ö ¸ğµÎ À½¼ö°ª°ú ÅØ½ºÆ® µ¥ÀÌÅÍ¸¦ Æ÷ÇÔÇÏ±â ¶§¹®¿¡ ÅØ½ºÆ® µ¥ÀÌÅÍ Á¦¿Ü ¹× À½¼ö°ª Ã³¸®°¡ ÇÊ¿äÇÔ.

#ÅØ½ºÆ® µ¥ÀÌÅÍ Á¦°Å
card <- filter(card,!str_detect(selng_cascnt, regex("[¤¡-¤¾°¡-ÆR]")))#ÅØ½ºÆ® µ¥ÀÌÅÍ ¾ø¾Ö±â
card <- filter(card,!str_detect(salamt, regex("[¤¡-¤¾°¡-ÆR]")))#ÅØ½ºÆ® µ¥ÀÌÅÍ ¾ø¾Ö±â

#¼ıÀÚ µ¥ÀÌÅÍ ¼ıÀÚÇü½ÄÀ¸·Î ÁöÁ¤ ¹× À½¼ö°ª Àı´ë°ªÀ» ÅëÇØ ¾ç¼ö °ªÀ¸·Î Ã³¸®
card$selng_cascnt<-as.numeric(card$selng_cascnt)#¼ıÀÚ µ¥ÀÌÅÍ·Î ¹Ù²Ù±â
card$salamt<-as.numeric(card$salamt)#¼ıÀÚ µ¥ÀÌÅÍ·Î ¹Ù²Ù±â
card$selng_cascnt<-abs(card$selng_cascnt)#À½¼ö°ª Ã³¸®ÇÏ±â À§ÇØ Àı´ë°ª Ã³¸®ÇØÁÜ
card$salamt<-abs(card$salamt)#À½¼ö°ª Ã³¸®ÇÏ±â À§ÇØ Àı´ë°ª Ã³¸®ÇØÁÜ

#cardÀÇ mrhst_industry_cl_code´Â °¡¸ÍÁ¡ ¾÷Á¾ÄÚµå 
V2 <- substr(card$mrhst_induty_cl_code,1,1)
V2
card<-cbind(card, V2)#industryÄÚµåÀÇ ¾ÕºÎºĞÀº Ä«Å×°í¸®¸¦ ¶æÇÏ¹Ç·Î ³ª´®
#Ä«Å×°í¸®ÇüÀ» characterÇüÀ¸·Î º¯È¯
card$V2<-gsub("1","¿©Çà&±³Åë¼ö´Ü",card$V2)
card$V2<-gsub("2",'½ºÆ÷Ã÷&¹®È­&¿©°¡',card$V2)
card$V2<-gsub("3",'»ıÈ°¿ëÇ°&ÁÖÀ¯',card$V2)
card$V2<-gsub("4",'ÆĞ¼Ç&¼îÇÎ',card$V2)
card$V2<-gsub("5",'±³À°&»ç¹«',card$V2)
card$V2<-gsub("6",'Â÷·®&º¸Çè',card$V2)
card$V2<-gsub("7",'ÀÇ·á&¹Ì¿ë',card$V2)
card$V2<-gsub("8",'½ÄÇ°&¿Ü½Ä',card$V2)
card$V2<-gsub("9",'±âÅ¸',card$V2)

card<-cbind(card,V2)#À§¿¡ ÀÌ¸§ÇüÀ¸·Î ¹Ù²ãÁØ°Í º¯¼öÈ­ ÇÏ°í, ¿­¿¡ Ãß°¡ÇØÁØ°ÍÀÓ
card<-cbind(card, substr(card$receipt_dttm,5,6))#µ¥ÀÌÅÍ¸¦ ¿ùº°·Î º¸°íÀÚ, 20200225¶ó¸é 02(¿ù)¸¸ »©¼­ º¯¼öÈ­ ½ÃÄÑÁÖ°í, ¸¶Âù°¡Áö·Î ¿­¿¡ Ãß°¡
names(card) <- c('date', 'ad_code', 'ad_nm', 'ind_code', 'ind_nm', 'sal_cnt', 'sal_amt', 'cate_name','cate_code','month') #º¯¼ö ÀÌ¸§ Çò°¥·Á¼­ ½±°Ô ¹Ù²Ş


install.packages("devtools")
install.packages("lubridate")
library(lubridate)

#Ä«Å×°í¸®º° µ¥ÀÌÅÍ ³ª´©±â

for (i in 1:9){assign(paste0("card",i),filter(card, cate_code == i))}
card$sal_cnt <- as.numeric(card$sal_cnt)
card$sal_amt <- as.numeric(card$sal_amt)

#Ä«Å×°í¸®º° ¿ùº° °áÁ¦°Ç¼ö °è»ê
for (x in 1:9){
  A<-filter(card, cate_code == x)
  assign(paste0("category1",x),aggregate(sal_cnt~month,A,sum))
}

#Ä«Å×°í¸®º° ¿ùº° °áÀç±İ¾× °è»ê
for (x in 1:9){
  A<-filter(card, cate_code == x)
  assign(paste0("category2",x),aggregate(sal_amt~month,A,sum))
}

# ³¯Â¥ ÄÃ·³ ³¯Â¥ Çü½ÄÀ¸·Î º¯È¯
card$date <- ymd(card$date)

#card table ¸¸µé±â
view_card <- card[,-10]
colnames(view_card) <- c("³¯Â¥", "ÇàÁ¤µ¿ÄÚµå","ÇàÁ¤µ¿¸í","°¡¸ÍÁ¡¾÷Á¾ÄÚµå","°¡¸ÍÁ¡¾÷Á¾¸í","¸ÅÃâ¹ß»ı°Ç¼ö","¸ÅÃâ¹ß»ı±İ¾×","°¡¸ÍÁ¡ Ä«Å×°í¸®","Ä«Å×°í¸®ÀÌ¸§")

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

# ³¯Â¥º° ¸ÅÃâ°Ç¼ö, ¸ÅÃâ¾× º¯È­
card.date <- group_by(card, receipt_dttm) %>% summarize(day_selng_cascnt = mean(selng_cascnt),
                                                        day_salamt = mean(salamt))
head(card.date)

# ³¯Â¥º° Æò±Õ ¸ÅÃâ °Ç¼ö¿Í ¸ÅÃâ¾× º¯È­ 

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

# ¸ÅÃâ°Ç¼ö¿Í ¸ÅÃâ¾×ÀÌ Àü¹İÀûÀ¸·Î 1¿ù ¸» ~ 2¿ù ÃÊ¸¦ ±âÁØÀ¸·Î °¨¼ÒÇÑ °ÍÀ» ¾Ë ¼ö ÀÖÀ½





