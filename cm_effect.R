# ---------------
#  2019.10.16
# ---------------

# データの読み込み
data_1 = read.csv("C:/Users/lixin/Desktop/sozu/Rによるデータ解析/RawData.csv", row.names = 1)

# パッケージの呼び出し
library(ggplot2)
library(reshape2)

# 認知度向上に対応する変数を作る
table(data_1$Drink_2_PI_1, data_1$Drink_2_PI_2)
data_1$Drink_2_PI_upp = ifelse(data_1$Drink_2_PI_1 %in% c(1, 2),
                               pmax(sign(data_1$Drink_2_PI_2 - data_1$Drink_2_PI_1), 0),
                               pmax(sign(data_1$Drink_2_PI_2 + 1 - data_1$Drink_2_PI_1), 0))


# 年齢5歳刻みとDrink2の認知度向上について集計と視覚化
table(data_1$Drink_2_PI_upp, data_1$年齢_5歳刻み)
round(prop.table(table(data_1$Drink_2_PI_upp, data_1$年齢_5歳刻み), margin = 2) * 100, 3)
ggplot(data_1, aes(x = as.factor(年齢_5歳刻み), fill = as.factor(Drink_2_PI_upp))) + 
  geom_bar(position = 'fill') + labs(fill = '購買意欲向上の有無') + xlab("年齢") + coord_flip()

# 性別とDrink2の認知度向上について分割表を作成する
table(data_1$Drink_2_PI_upp, data_1$Sex)
round(prop.table(table(data_1$Drink_2_PI_upp, data_1$Sex), margin = 2) * 100, 3)
ggplot(data_1, aes(x = as.factor(Sex), fill = as.factor(Drink_2_PI_upp))) + 
  geom_bar(position = 'fill') + labs(fill = '購買意欲向上の有無') + xlab("性別") + coord_flip()

# 年収とDrink2の認知度向上について分割表を作成する
table(data_1$Drink_2_PI_upp, data_1$年収)
round(prop.table(table(data_1$Drink_2_PI_upp, data_1$年収), margin = 2) * 100, 3)
ggplot(data_1, aes(x = as.factor(年収), fill = as.factor(Drink_2_PI_upp))) + 
  geom_bar(position = 'fill') + labs(fill = '購買意欲向上の有無') + xlab("年収") + coord_flip()

# ---------------------------------------------------------------------------------------

# 購買行動向上に対応する変数を作る
table(data_1$Drink_2_PS_1, data_1$Drink_2_PS_2)
data_1$Drink_2_PS_upp = ifelse(data_1$Drink_2_PS_1 %in% c(1, 2),
                               pmax(sign(data_1$Drink_2_PS_2 - data_1$Drink_2_PS_1), 0),
                               pmax(sign(data_1$Drink_2_PS_2 + 1 - data_1$Drink_2_PS_1), 0))

# 年齢5歳刻みとDrink2の購買行動向上について集計と視覚化
table(data_1$Drink_2_PS_upp, data_1$年齢_5歳刻み)
round(prop.table(table(data_1$Drink_2_PS_upp, data_1$年齢_5歳刻み), margin = 2) * 100, 3)
ggplot(data_1, aes(x = as.factor(年齢_5歳刻み), fill = as.factor(Drink_2_PS_upp))) + 
  geom_bar(position = 'fill') + labs(fill = '購買行動向上の有無') + xlab("年齢") + coord_flip()

# 性別とDrink2の購買行動向上について分割表を作成する
table(data_1$Drink_2_PS_upp, data_1$Sex)
round(prop.table(table(data_1$Drink_2_PS_upp, data_1$Sex), margin = 2) * 100, 3)
ggplot(data_1, aes(x = as.factor(Sex), fill = as.factor(Drink_2_PS_upp))) + 
  geom_bar(position = 'fill') + labs(fill = '購買行動向上の有無') + xlab("性別") + coord_flip()

# 年収とDrink2の購買意欲向上について分割表を作成する
table(data_1$Drink_2_PS_upp, data_1$年収)
round(prop.table(table(data_1$Drink_2_PS_upp, data_1$年収), margin = 2) * 100, 3)
ggplot(data_1, aes(x = as.factor(年収), fill = as.factor(Drink_2_PS_upp))) + 
  geom_bar(position = 'fill') + labs(fill = '購買意欲向上の有無') + xlab("年収") + coord_flip()

# ---------------------------------------------------------------------------------------

# CM視聴回数の作成と集計
data_1$Dr2_count = rowSums(data_1[ , grep('D2_', colnames(data_1))])
summary(data_1$Dr2_count)

# データを男女別で分ける
data_1$Drink_2_PI_upp_sex = dplyr::case_when(
  data_1$Drink_2_PI_upp == 0 & data_1$Sex == 0 ~ "女性+向上なし",
  data_1$Drink_2_PI_upp == 0 & data_1$Sex == 1 ~ "男性+向上なし",
  data_1$Drink_2_PI_upp == 1 & data_1$Sex == 0 ~ "女性+向上あり",
  data_1$Drink_2_PI_upp == 1 & data_1$Sex == 1 ~ "男性+向上あり")

# 男女別CM視聴回数と認知度向上の有無(バイオリンプロット)
tapply(X = data_1$Dr2_count, INDEX = data_1$Drink_2_PI_upp_sex, FUN = summary)
ggplot(data_1, aes(x = as.factor(Drink_2_PI_upp_sex), y = Dr2_count, 
                   fill = as.factor(Drink_2_PI_upp_sex)) ) + 
  geom_violin() + 
  geom_boxplot(width = .1, fill = "black") + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 2.5) + 
  labs(fill = "男女別購買意欲向上の有無") + xlab("男女別購買意欲向上の有無") + ylab("CM視聴回数")

# ---------------------------------------------------------------------------------------
# 購買行動

# データを男女別で分ける
data_1$Drink_2_PS_upp_sex = dplyr::case_when(
  data_1$Drink_2_PS_upp == 0 & data_1$Sex == 0 ~ "女性+向上なし",
  data_1$Drink_2_PS_upp == 0 & data_1$Sex == 1 ~ "男性+向上なし",
  data_1$Drink_2_PS_upp == 1 & data_1$Sex == 0 ~ "女性+向上あり",
  data_1$Drink_2_PS_upp == 1 & data_1$Sex == 1 ~ "男性+向上あり")

# 男女別CM視聴回数と購買行動向上の有無(バイオリンプロット)
tapply(X = data_1$Dr2_count, INDEX = data_1$Drink_2_PS_upp_sex, FUN = summary)
ggplot(data_1, aes(x = as.factor(Drink_2_PS_upp_sex), y = Dr2_count, 
                   fill = as.factor(Drink_2_PS_upp_sex)) ) + 
  geom_violin() + 
  geom_boxplot(width = .1, fill = "black") + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 2.5) + 
  labs(fill = "男女別購買行動向上の有無") + xlab("男女別購買行動向上の有無") + ylab("CM視聴回数")

# ---------------------------------------------------------------------------------------

# 個人ごとにTV視聴に占めるCMを視聴した割合(CM視聴回数/TV視聴回数)
# を変数として作り，認知度向上との関連を確認せ
data_1$Percentage = rowSums(data_1[ , grep('D2_', colnames(data_1))]) / rowSums(data_1[, grep('CH_', colnames(data_1))])
summary(data_1$Percentage)

# 男女別
tapply(X = data_1$Percentage, INDEX = data_1$Drink_2_PI_upp_sex, FUN = summary)
ggplot(data_1, aes(x = as.factor(Drink_2_PI_upp_sex), y = Percentage, 
                   fill = as.factor(Drink_2_PI_upp_sex)) ) + 
  geom_violin() + 
  geom_boxplot(width = .1, fill = "black") + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 2.5) + 
  labs(fill = "男女別購買意欲向上の有無") + xlab("男女別購買意欲向上の有無") + ylab("個人ごとにTV視聴に占めるCMを視聴した割合")

# 購買行動
tapply(X = data_1$Percentage, INDEX = data_1$Drink_2_PS_upp_sex, FUN = summary)
ggplot(data_1, aes(x = as.factor(Drink_2_PS_upp_sex), y = Percentage, 
                   fill = as.factor(Drink_2_PS_upp_sex)) ) + 
  geom_violin() + 
  geom_boxplot(width = .1, fill = "black") + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 2.5) + 
  labs(fill = "男女別購買行動向上の有無") + xlab("男女別購買行動向上の有無") + ylab("個人ごとにTV視聴に占めるCMを視聴した割合")

# ---------------------------------------------------------------------------------------

# ロジスティック回帰分析
# データの準備
data_2 = data_1[ , c("Drink_2_PI_upp", "年齢_5歳刻み", "Sex", "年収", "Dr2_count")]
data_2$Drink_2_PI_upp = as.factor(data_2$Drink_2_PI_upp)
data_2$年齢_5歳刻み = as.factor(data_2$年齢_5歳刻み)
data_2$年収 = as.factor(data_2$年収)

# ロジスティック回帰分析の運用
fitl = glm(Drink_2_PI_upp ~ 年齢_5歳刻み + Sex + 年収 + Dr2_count,
           data = data_2, family = 'binomial')
summary(fitl) # 結果の要約を出力
confint(fitl) # 回帰係数の95%信頼空間の出力

# ---------------------------------------------------------------------------------------

# ロジスティック回帰分析
# データの準備
data_3 = data_1[ , c("Drink_2_PS_upp", "Drink_2_PI_upp", "年齢_5歳刻み", "Sex", "年収", "Dr2_count")]
data_3$Drink_2_PS_upp = as.factor(data_3$Drink_2_PS_upp)
data_3$Drink_2_PI_upp = as.factor(data_3$Drink_2_PI_upp)
data_3$年齢_5歳刻み = as.factor(data_3$年齢_5歳刻み)
data_3$年収 = as.factor(data_3$年収)

# ロジスティック回帰分析の運用
fitl = glm(Drink_2_PS_upp ~ Drink_2_PI_upp + 年齢_5歳刻み + Sex + 年収 + Dr2_count,
           data = data_3, family = 'binomial')
summary(fitl) # 結果の要約を出力
confint(fitl) # 回帰係数の95%信頼空間の出力

# ---------------------------------------------------------------------------------------

BIG_5_item = data_1[, 4:43] # data_1の4~43列目を代入
data_1$Big5_1 = apply(data_1[, grep('Big5_1_', colnames(data_1), fixed = T)], 1, sum)
data_1$Big5_2 = apply(data_1[, grep('Big5_2_', colnames(data_1), fixed = T)], 1, sum)
data_1$Big5_3 = apply(data_1[, grep('Big5_3_', colnames(data_1), fixed = T)], 1, sum)
data_1$Big5_4 = apply(data_1[, grep('Big5_4_', colnames(data_1), fixed = T)], 1, sum)
data_1$Big5_5 = apply(data_1[, grep('Big5_5_', colnames(data_1), fixed = T)], 1, sum)

# Big5をカテゴリごとに集結する
#data_1$BIG_5_category = data_1[, c('Big5_1', 'Big5_2', 'Big5_3', 'Big5_4', 'Big5_5')]
data_1$BIG_5_category =  apply(data_1[ , c('Big5_1', 'Big5_2', 'Big5_3', 'Big5_4', 'Big5_5')], 1, sum)

ggplot(data_1, aes(x = as.factor(Sex), y = Big5_1, fill = as.factor(Sex)) ) +
  geom_violin() + geom_boxplot(width = .1, fill = "black") + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 2.5) + 
  labs(fill = "性別") + 
  xlab("性別カテゴリ") +
  ylab("Big5_1の合計得点")

ggplot(data_1, aes(x = as.factor(Sex), y = Big5_2, fill = as.factor(Sex))) +
  geom_violin() + geom_boxplot(width = .1, fill = "black") + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 2.5) + 
  labs(fill = "性別") + 
  xlab("性別カテゴリ") +
  ylab("Big5_2の合計得点")

ggplot(data_1, aes(x = as.factor(Sex), y = Big5_3, fill = as.factor(Sex))) +
  geom_violin() + geom_boxplot(width = .1, fill = "black") + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 2.5) + 
  labs(fill = "性別") + 
  xlab("性別カテゴリ") +
  ylab("Big5_3の合計得点")

ggplot(data_1, aes(x = as.factor(Sex), y = Big5_4, fill = as.factor(Sex))) +
  geom_violin() + geom_boxplot(width = .1, fill = "black") + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 2.5) + 
  labs(fill = "性別") + 
  xlab("性別カテゴリ") +
  ylab("Big5_4の合計得点")

ggplot(data_1, aes(x = as.factor(Sex), y = Big5_5, fill = as.factor(Sex))) +
  geom_violin() + geom_boxplot(width = .1, fill = "black") + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 2.5) + 
  labs(fill = "性別") + 
  xlab("性別カテゴリ") +
  ylab("Big5_5の合計得点")


