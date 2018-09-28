rm(list=lsf.str())

setwd("C:\\Users\\NK-1\\Documents\\GitHub\\behavior-segments-pl-gorod")

# open files, format, rename and filter
library(rio)
convert("transactions-data.xlsx", "transactions-data.csv")
convert("fraud-data.xlsx", "fraud-data.csv")
convert("partners_directory.xlsx", "partners_directory.csv")


client_list <- read.csv("client-data.csv", encoding = "UTF-8", stringsAsFactors = FALSE, sep = ';')
master_list1 <- read.csv("transactions-data.csv", stringsAsFactors = FALSE,  encoding = "UTF-8")
fraud <- read.csv("fraud-data.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
bq_list <- read.csv("web-site-and-app-data.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
partner_cat1 <- read.csv("partners_directory.csv", encoding = "UTF-8", stringsAsFactors = T, sep = ',',
                        colClasses = rep('character'))

library(dplyr)
client_list <- client_list %>%
  rename(userid='X.U.FEFF.User.ID',
         client_type='Тип.клиентского.приложения',
         level='Уровень',
         reg_date='Дата.регистрации') %>%
  mutate(userid=as.character(userid),
         client_type=as.character(client_type),
         level=as.integer(level))

fraud <- fraud %>%
  rename(userid='User.ID')

names(master_list1)

master_list <- master_list1 %>%
  rename(userid='User.ID',
         date_cassa='Дата.Касса.',
         date_procesing='Дата',
         type_partn='Тип.партнера',
         partner='Партнер.Группа.',
         regs_in_per='Кол.во.регистраций.за.период...чел.',
         qt_trans='Кол.вo.транзакций..ед.',
         sum_trans='Сумма.покупок.деньги.бонусы...руб.',
         sum_trans_money='Сумма.покупок.деньги...руб.',
         rate_inc_bon='Средний.процент.начисления',
         share_dec_bon='Средний.процент.списания',
         sum_inc_bon='Сумма.начисленных.бонусов..руб.',
         sum_inc_bon_cor='Сумма.начислений..с.корректировками.',
         sum_dec_bon='Сумма.списанных.бонусoв..руб.',
         sum_inc_bon_par='Суммa.начисленная.партнером..руб.',
         rate_inc_bon_par='Начислено.партнером...')  %>%
  filter(type_partn!='Метро') %>%
  filter(userid!='-') %>%
  filter(!(userid %in% fraud$userid)) %>%
  mutate(userid=as.character(userid),
         rate_inc_bon=as.numeric(rate_inc_bon),
         share_dec_bon=as.numeric(share_dec_bon),
         rate_inc_bon_par=as.numeric(rate_inc_bon_par),
         users_with_trans=ifelse(qt_trans>0,1,0)) %>%
  mutate(date_cassa=gsub('T', ' ', date_cassa),
         date_cassa=gsub('Z', '', date_cassa),
         date_cassa=as.Date(date_cassa, format='%Y-%m-%d %H:%M:%S'),
         date_procesing=gsub('T', ' ', date_procesing),
         date_procesing=gsub('Z', '', date_procesing),
         date_procesing=as.Date(date_procesing, format='%Y-%m-%d %H:%M:%S')) %>%
  filter(date_cassa > '2018-08-03' & date_cassa < '2018-09-04') %>%
  filter(sum_trans>-0.99) ## почему в исходнике отрицательная сумма транзакции?
         

# merge trandactions data with clients- and web-and-app data
all_qlick_tab <- merge(master_list, client_list, all.x = T, all.y = T, by = c('userid'))
all_tab <- merge(all_qlick_tab, bq_list, all.x = T, all.y= T, by.x = 1, by.y = 1)

# check number of rows
dim(client_list)
dim(master_list)
dim(all_qlick_tab)
dim(all_tab)

## remove na
all_tab_withoutna <- all_tab[,-c(2,3)] ##exclude columns with dates
all_tab_withoutna[is.na(all_tab_withoutna)] <- 0

# create segments
visits_group <- all_tab_withoutna  %>%
  group_by(sessions_in_web, sessions_in_app) %>%
  summarise(users=n(),
            users_with_trans=sum(users_with_trans),
            avg_sessions_total=sum(sessions_in_web+sessions_in_app)/sum(users),
            avg_bill=sum(sum_trans)/sum(qt_trans),
            avg_trans_per_user=sum(qt_trans)/sum(users_with_trans),
            share_dec_bon=mean(share_dec_bon),
            rate_inc_bon_par=mean(rate_inc_bon_par),
            rate_inc_bon=mean(rate_inc_bon),
            delta_rates=rate_inc_bon_par-rate_inc_bon,
            level=mean(level))

set.seed(123)
visits_group$segments <- kmeans(visits_group[,c('sessions_in_web', 'avg_sessions_total', 'sessions_in_app')], 7, nstart = 30)$cluster

visits_group$segments[visits_group$sessions_in_web==0 & visits_group$sessions_in_app==0] <- '0 visits'
visits_group$segments[visits_group$sessions_in_web==0 & visits_group$sessions_in_app==1] <- '1 visit'
visits_group$segments[visits_group$sessions_in_web==1 & visits_group$sessions_in_app==0] <- '1 visit'
visits_group$segments[visits_group$segments==4 | visits_group$segments==6] <- 'App++'
visits_group$segments[visits_group$segments==5] <- 'Web++'
visits_group$segments[visits_group$segments==3 ] <- 'App+'
visits_group$segments[visits_group$segments==2 ] <- 'Web+'
visits_group$segments[visits_group$segments==7 ] <- 'Web++ App ++'
visits_group$segments[visits_group$segments==1 ] <- 'Some visits'

sapply(split(visits_group[,c(1,2)], visits_group$segments), colMeans) ## среднее значение по сегментам

## see users on plot by segments
library(ggplot2)
visits_group_p <- ggplot(visits_group, 
                         aes(sessions_in_web, sessions_in_app, 
                             color = as.factor(segments), 
                             size=avg_sessions_total)) + 
  geom_point() + theme_classic() +
  labs(title = "Сегменты по посещаемости сайта и приложений",
       y = "Количество посещений APP",
       x = "Количество посещений WEB",
       color='Сегменты',
       size='Количество посещений. всего')
  
visits_group_p 


# merge all data with segment group all data by segments for plots
calc_data <- merge(all_tab_withoutna, 
                   visits_group[,c('sessions_in_web', 'sessions_in_app','avg_sessions_total', 'segments')],
                   by.x = c('sessions_in_web', 'sessions_in_app'),
                   by.y = c('sessions_in_web', 'sessions_in_app'))

segment_tab <- calc_data %>%
  group_by(segments) %>%
  summarise(users=n(),
            users_with_trans=length(unique(userid[qt_trans>0])),
            avg_sessions_total=sum(sessions_in_web+sessions_in_app)/sum(users),
            avg_sessions_in_web=sum(sessions_in_web)/sum(users),
            avg_sessions_in_app=sum(sessions_in_app)/sum(users),
            avg_bill=sum(sum_trans)/sum(qt_trans),
            avg_trans_per_user=sum(qt_trans)/sum(users_with_trans),
            share_dec_bon=mean(share_dec_bon),
            rate_inc_bon_par=mean(rate_inc_bon_par),
            rate_inc_bon=mean(rate_inc_bon),
            delta_rates=rate_inc_bon_par-rate_inc_bon,
            level=mean(level),
            sum_trans=sum(sum_trans),
            qt_trans=sum(qt_trans))

segment_tab$segments <- as.factor(segment_tab$segments)

# build plots 
## 1
g1 <- ggplot(data=segment_tab, 
                aes(x=factor(segment_tab$segments, levels(segment_tab$segments)[c(1,4,5,2,6,3,7)]), 
                    y=avg_bill, fill=segments))+
  geom_col(position = "dodge", width=.7) +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = "Средний чек, руб.",
       y = NULL,
       x = "Сегменты",
       fill='Сегменты') +
  geom_text(aes(label = paste0(format(round(avg_bill/1000,digits=1),nsmall = 1),"K"),
                y= avg_bill), stat="identity", position = position_dodge(width=.7),
            vjust = -.3)


## 2
g2 <- ggplot(data=segment_tab, 
             aes(x=factor(segment_tab$segments, levels(segment_tab$segments)[c(1,4,5,2,6,3,7)]), 
                 y=avg_trans_per_user, fill=segments))+
  geom_col(position = "dodge", width=.7) +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = "Ср. кол-во транзакций на пользователя",
       y = NULL,
       x = "Сегменты",
       fill='Сегменты') +
  geom_text(aes(label = round(avg_trans_per_user,digits=1),
                y= avg_trans_per_user), stat="identity", position = position_dodge(width=.7),
            vjust = -.3, size=3)

## 3
g3 <- ggplot(data=segment_tab, 
             aes(x=factor(segment_tab$segments, levels(segment_tab$segments)[c(1,4,5,2,6,3,7)]), 
                 y=share_dec_bon, fill=segments))+
  geom_col(position = "dodge", width=.7) +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = "Ср. % списания пользователей от суммы покупки",
       y = NULL,
       x = "Сегменты",
       fill='Сегменты') +
  geom_text(aes(label = paste0(round(share_dec_bon*100,digits=1),"%"),
                y= share_dec_bon), stat="identity", position = position_dodge(width=.7),
            vjust = -.3)

## 4
g4 <- ggplot(data=segment_tab, 
             aes(x=factor(segment_tab$segments, levels(segment_tab$segments)[c(1,4,5,2,6,3,7)]), 
                 y=users, fill=segments))+
  geom_col(position = "dodge", width=.7) +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = "Количество пользователей в сегментах",
       y = NULL,
       x = "Сегменты",
       fill='Сегменты') +
  geom_text(aes(label = users,
                y= users), stat="identity", position = position_dodge(width=.7),
            vjust = -.3)

g4_u <- ggplot(data=segment_tab, 
             aes(x=factor(segment_tab$segments, levels(segment_tab$segments)[c(1,4,5,2,6,3,7)]), 
                 y=users_with_trans/users, fill=segments))+
  geom_col(position = "dodge", width=.7) +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = "Доля пользователей с транзакциями",
       subtitle="(от всех пользователей в сегменте)",
       y = NULL,
       x = "Сегменты",
       fill='Сегменты') +
  geom_text(aes(label = paste0(round(users_with_trans/users*100,1),"%"),
                y= users_with_trans/users), stat="identity", position = position_dodge(width=.7),
            vjust = -.3, size=3)

## 4_a
library(reshape2)
segment_melt <- melt(segment_tab)
serment_melt_cut <- segment_melt[segment_melt$variable %in% c('share_dec_bon', 'rate_inc_bon', 'rate_inc_bon_par', 'delta_rates'),]
levels(serment_melt_cut$variable)[c(8,9,10,11)] <- 
  c('Доля списания от суммы покупки', 'Ср. % начисления партнерами', 'Ср. % начисления пользователям', 'Разница в %% начисления')

serment_melt_lev <- segment_melt[segment_melt$variable %in% c('level'),]
levels(serment_melt_lev$variable)[12] <- 
  c('Уровень')

normalizer <- max(serment_melt_cut$value) / max(serment_melt_lev$value)

g4_a <- ggplot(data=serment_melt_cut, 
               aes(x=factor(serment_melt_cut$segments, levels(serment_melt_cut$segments)[c(1,4,5,2,6,3,7)]), 
                   y=value/normalizer, fill=variable))+
  geom_col(position = "dodge", width=.7) +
  geom_text(aes(label = paste0(round(value*100,digits=1),"%"),
                y= value/normalizer), stat="identity", position = position_dodge(width=.7),
            vjust = -.3, size=3) + 
  geom_line(data = serment_melt_lev, 
            size=1.3,
            color='#996600',
            aes(x=segments, y=value, group = 1)) +
  geom_text(data = serment_melt_lev, aes(x=segments, y=value, group = 1, 
                                         label = round(value,1),
                                         vjust = -.4),
            size=3) +
  scale_y_continuous(sec.axis = sec_axis(trans= ~.*normalizer,
                                         name = 'Уровень')) +
  theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  labs(title = "Проценты списания и начисления",
       y = "Процент,%",
       x = "Сегменты",
       fill='Показатель')

# merge all data with partners categories and build plots by partners categories

partner_cat <- partner_cat1 
calc_data_advanced <- merge(calc_data, partner_cat, all.x=T, by.x=c('ID.ТСП'), 
                            by.y = c('Partner.ID'))

calc_part_data <- calc_data_advanced %>%
  group_by(Category, segments) %>%
  summarise(users=n(),
            avg_sessions_total=sum(sessions_in_web+sessions_in_app)/sum(users),
            avg_sessions_in_web=sum(sessions_in_web)/sum(users),
            avg_sessions_in_app=sum(sessions_in_app)/sum(users),
            avg_bill=sum(sum_trans)/sum(qt_trans),
            avg_trans_per_user=sum(qt_trans)/sum(users_with_trans),
            share_dec_bon=mean(share_dec_bon),
            rate_inc_bon_par=mean(rate_inc_bon_par),
            rate_inc_bon=mean(rate_inc_bon),
            delta_rates=rate_inc_bon_par-rate_inc_bon,
            level=mean(level),
            sum_trans=sum(sum_trans),
            users_with_trans=length(unique(userid[qt_trans>0])),
            qt_trans=sum(qt_trans))


calc_part_data_full <- merge(calc_part_data, segment_tab, all.x = T, by = c('segments'))

calc_part_data_procent <- mutate(calc_part_data_full,
                                 sum_trans=sum_trans.x/sum_trans.y,
                                 users_with_trans=users_with_trans.x/users_with_trans.y,
                                 qt_trans=qt_trans.x/qt_trans.y)

## order factor levels
calc_part_data_procent$Category <- as.factor(calc_part_data_procent$Category)

calc_part_data_procent$Category <- factor(calc_part_data_procent$Category,levels(calc_part_data_procent$Category))


calc_part_data_procent <- calc_part_data_procent[complete.cases(calc_part_data_procent),]

## 
p1 <- ggplot(calc_part_data_procent, 
       aes(x = "", y=sum_trans, fill = factor(Category, levels = levels(Category)[c(6,4,5,1,3,2,7:12)]))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Категория", 
       x=NULL, 
       y=NULL, 
       title="Общая сумма покупок") + facet_wrap(segments~.)  + coord_polar(theta = "y", start=0,
                                                                           direction = -1) + 
  geom_text(aes(label=ifelse(sum_trans>0.08,paste0(round(sum_trans*100,1),'%'),
                             ''),
                y=sum_trans),
            position = position_stack(vjust = 0.4),
            size=2.5) 
  
##
p2 <- ggplot(calc_part_data_procent, 
             aes(x = "", y=qt_trans, fill=factor(Category, levels = levels(Category)[c(6,4,5,1,3,2,7:12)])))  + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Категория", 
       x=NULL, 
       y=NULL, 
       title="Количество транзакций") + facet_wrap(segments~.)  + coord_polar(theta = "y", start=0,
                                                                           direction = -1, clip='off') + 
  geom_text(aes(label=ifelse(qt_trans>0.08,paste0(round(qt_trans*100,1),'%'),
                             ''),
                y=qt_trans),
            position = position_stack(vjust = 0.5),
            size=2.5) 

## see users on plot by type of registration
visits_group_c <- calc_data_advanced  %>%
  group_by(sessions_in_web, sessions_in_app, client_type, segments) %>%
  summarise(users=n(),
            users_with_trans=sum(users_with_trans),
            avg_sessions_total=sum(sessions_in_web+sessions_in_app)/sum(users),
            avg_bill=sum(sum_trans)/sum(qt_trans),
            avg_trans_per_user=sum(qt_trans)/sum(users_with_trans),
            share_dec_bon=mean(share_dec_bon),
            rate_inc_bon_par=mean(rate_inc_bon_par),
            rate_inc_bon=mean(rate_inc_bon),
            delta_rates=rate_inc_bon_par-rate_inc_bon,
            level=mean(level))

visits_group_c <- as.data.frame(visits_group_c)
visits_group_c$client_type <- as.factor(visits_group_c$client_type)
levels(visits_group_c$client_type) <- c('App', 'App', 'Web')

visits_group_plot <- ggplot(data=visits_group_c, 
                            aes(sessions_in_web, sessions_in_app, 
                                color = as.factor(client_type))) + 
  geom_point() + theme_classic() +
  labs(title = "Сегменты по посещаемости сайта и приложений",
       y = "Количество посещений APP",
       x = "Количество посещений WEB",
       color='Сегменты',
       size='Количество посещений. всего')+ facet_wrap(client_type+segments~.)

visits_group_plot 

## all plots again
visits_group_p

g1 + scale_fill_manual(values=c("#996600", "#CCCC66", "#669900", "#FFCC00", "#CC6600", 
                                "#CC9933", "#666633"))

g2 + scale_fill_manual(values=c("#996600", "#CCCC66", "#669900", "#FFCC00", "#CC6600", 
                                "#CC9933", "#666633"))

g3 + scale_fill_manual(values=c("#996600", "#CCCC66", "#669900", "#FFCC00", "#CC6600", 
                                "#CC9933", "#666633"))

g4 + scale_fill_manual(values=c("#996600", "#CCCC66", "#669900", "#FFCC00", "#CC6600", 
                                "#CC9933", "#666633"))

g4_u + scale_fill_manual(values=c("#996600", "#CCCC66", "#669900", "#FFCC00", "#CC6600", 
                                  "#CC9933", "#666633"))

g4_a + scale_fill_manual(values=c("#996600", "#CCCC66", "#669900", "#FFCC00", "#CC6600", 
                                  "#CC9933", "#666633"))

p1 + scale_fill_brewer(palette = 'Spectral') ##RdYlGn

p2 + scale_fill_brewer(palette = 'Spectral') ##RdYlGn


## cluster plot (don't use on presentation)
library(factoextra)
fviz_cluster(kmeans(all_tab_withoutna[,c('sessions_in_web', 'sessions_in_app')], 7, nstart = 30),
             data = all_tab_withoutna[,c('sessions_in_web', 'sessions_in_app')]) ##cluster plot

## all colors http://websitetips.com/colorcharts/visibone/hex/