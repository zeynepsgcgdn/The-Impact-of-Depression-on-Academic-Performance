
rm(list = ls())
library(readxl)

#veriyi yükleme
ist347_odev_41 <- read_excel(file.choose())

library(skimr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(reshape2)
library(psych)
library(moments)
library(e1071)
#veriyi kontrol etmek için
View(ist347_odev_41)
#gözlem sayısını verir
nrow(ist347_odev_41)
#değişken sayısını verir
ncol(ist347_odev_41)
#kategorik değişkenleri faktöre çevirelim
ist347_odev_41$cinsiyet <- as.factor(ist347_odev_41$cinsiyet)
ist347_odev_41$sehir <- as.factor(ist347_odev_41$sehir)
ist347_odev_41$meslek <- as.factor(ist347_odev_41$meslek)
ist347_odev_41$uyku_durumu <- as.factor(ist347_odev_41$uyku_durumu)
ist347_odev_41$diyet_aliskanlik <- as.factor(ist347_odev_41$diyet_aliskanlik)
ist347_odev_41$derece <- as.factor(ist347_odev_41$derece)
ist347_odev_41$sakinlestirici_ilac <- as.factor(ist347_odev_41$sakinlestirici_ilac)
ist347_odev_41$ailede_mental_hastalik_gecmisi <- as.factor(ist347_odev_41$ailede_mental_hastalik_gecmisi)
ist347_odev_41$depresyon_durumu<-as.factor(ist347_odev_41$depresyon_durumu)
#ilk inceleme
#head ilk 6 satırı gösterir
head(ist347_odev_41)
#son 6 satırı gösterir
tail(ist347_odev_41)
#tanımlayıcı istatistikleriinceleme
summary(ist347_odev_41)
#Analiz için ilginç yorumlamalar 
teacher_profil <- ist347_odev_41[ist347_odev_41$meslek == "Teacher", ]
t(teacher_profil)

diyetothers_profil <- ist347_odev_41[ist347_odev_41$diyet_aliskanlik == "Others", ]
t(diyetothers_profil)

#kapsamlı özeti almak için 
skim(ist347_odev_41)
#kayıp gözlemleri kontrol eder
colSums(is.na(ist347_odev_41))
#değişken türleri
# Veri setindeki sayısal sütunların isimlerini bulup 'num_vars' içine atar
num_vars <- names(ist347_odev_41)[sapply(ist347_odev_41, is.numeric)]
num_vars
# Kategorik değişkenleri seç
kategorik_degiskenler <- ist347_odev_41 %>% select(where(is.character) | where(is.factor))
names(kategorik_degiskenler)
# Değişken yapısını görelim
glimpse(ist347_odev_41)
#çarpıklık basıklık hesaplama
describe(ist347_odev_41)
# 2. Grafik ekranını 2 satır ve 2 sütuna bölelim
par(mfrow = c(2, 2))
#sayısal değişkenler için histogram çizelim
hist(ist347_odev_41$yas, main = "Yaş Dağılımı", xlab = "Yaş", col = "lightblue", border = "white")
hist(ist347_odev_41$akademik_baski, main = "Akademik Baskı", xlab = "Baskı Skoru", col = "salmon", border = "white")
hist(ist347_odev_41$ortalama_not, main = "Ortalama Not", xlab = "Not", col = "lightgreen", border = "white")
hist(ist347_odev_41$gunluk_calisma_saati, main = "Günlük Çalışma Saati", xlab = "Saat", col = "gold", border = "white")
# Ekranı 2 satır, 2 sütuna böl
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
# 1. Cinsiyet Dağılımı
barplot(table(ist347_odev_41$cinsiyet), main = "Cinsiyet", col = "skyblue", border = "white")
# 2. Meslek Dağılımı
barplot(table(ist347_odev_41$meslek), main = "Meslek Dağılımı", col = "tomato", border = "white")
# 3. Uyku Süresi
barplot(table(ist347_odev_41$uyku_durumu), main = "Uyku Süresi", col = "orchid", border = "white")
# 4. Şehir Dağılımı (Çok fazla şehir varsa yazıların sığması için las=2 ekleyebilirsiniz)
barplot(table(ist347_odev_41$sehir), main = "Şehirler", col = "gold", border = "white", las = 2)



#sürekli değişkenleri kategorik yapma
ist347_odev_41 <- ist347_odev_41 %>%
  mutate(calisma_saatleri = case_when(
    gunluk_calisma_saati < 5  ~ "Dusuk",
    gunluk_calisma_saati >= 5 & gunluk_calisma_saati < 10 ~ "Orta",
    gunluk_calisma_saati >= 10 ~ "Yuksek"
  ))
ist347_odev_41 <- ist347_odev_41 %>%
  mutate(yas_gruplari = case_when(
    yas <= 22 ~ "genc",               # 22 ve altı
    yas >= 23 & yas <= 26 ~ "orta_genc", # 23 ile 26 arası
    yas >= 27 ~ "yetiskin"            # 27 ve üzeri
  ))

#  count fonksiyonu ile grupları kontrol edelim
ist347_odev_41 %>%
  count(yas_gruplari)



#Soru1
# Mann-Whitney U Testi
# Depresyonu oranı 1 veya sıfır olan öğrencilerin akademik baskı düzeyleri arasındaki farka bak
wilcox.test(akademik_baski ~ depresyon_durumu, data = ist347_odev_41,alternative = "two.sided", conf.int = TRUE)

#grafik kodu
library(ggplot2)
ggplot(ist347_odev_41, aes(x = factor(depresyon_durumu), y = akademik_baski, fill = factor(depresyon_durumu))) +
  # Kutu Grafiği
  geom_boxplot() + 
   # Noktalar
  geom_jitter(width = 0.1, alpha = 0.3, color = "black") +
  # Etiketler
  labs(
    title = "Depresyon Durumuna Gore Akademik Baskı",
    x = "Depresyon Durumu",
    y = "Akademik Baskı Puanı",
    fill = "Grup"
  )+
  coord_flip()
  


#SORU2
# Spearman Korelasyonu 
# 1. Öğrenci tatmini ve Akademik Baskı ilişkisi
cor.test(ist347_odev_41$ogrenci_tatmini, ist347_odev_41$akademik_baski, method = "spearman",exact=FALSE)
#grafik

ggplot(ist347_odev_41, aes(x = akademik_baski, y = ogrenci_tatmini)) +
   # 1. Noktalar 
  geom_point(color = "black", alpha = 0.6) +
  
  # 2.  İlişkinin yönü
  geom_smooth( method=lm ,color = "red", fill = "pink") +
   # 3. Etiketler 
  labs(
    title = "Akademik Baskı ve Ogrenci Tatmini İliskisi",
    x = "Akademik Baski Puan",
    y = "Ogrenci Tatmin Puani")
  
#Soru3
# çalışma saatlerinini gruplara ayırdığımızda bir artış var mı?
library(dplyr)
ist347_odev_41 <- ist347_odev_41 %>%
  mutate(calisma_saatleri = case_when(
    gunluk_calisma_saati < 5  ~ "Dusuk",
    gunluk_calisma_saati >= 5 & gunluk_calisma_saati < 10 ~ "Orta",
    gunluk_calisma_saati >= 10 ~ "Yuksek"
  ))

count(ist347_odev_41, calisma_saatleri)
# 3. Ki-Kare Testi (Chi-Square)
# Çalışma grubu ile Depresyon arasındaki ilişki
tablo_calisma <- table(ist347_odev_41$calisma_saatleri, ist347_odev_41$depresyon_durumu)
print(tablo_calisma) # Tabloyu gör
chisq.test(tablo_calisma) # Testi yap
#GRAFİK
ist347_odev_41$calisma_saatleri <- factor(ist347_odev_41$calisma_saatleri, 
                                          levels = c("Dusuk", "Orta", "Yuksek"))
ggplot(ist347_odev_41, aes(x =calisma_saatleri,fill = depresyon_durumu)) +
  geom_bar(position = "fill", width=0.7, color="blue")

labs(
  title = "Akademik Baskı ve Ogrenci Tatmini İliskisi",
  x = "Akademik Baski Puan",
  y = "Ogrenci Tatmin Puani")

library(ggplot2)
#çalışma grupları düşükten yükseğe doğru gitmeli
ist347_odev_41$calisma_saatleri <- factor(ist347_odev_41$calisma_saatleri, 
                                          levels = c("Dusuk", "Orta", "Yuksek"))

ggplot(ist347_odev_41, aes(x = calisma_saatleri, fill = factor(depresyon_durumu))) +
  
  # position = "dodge" Sütunları yan yana koyar
  geom_bar(position = "dodge", color = "black") + 
  
  scale_fill_manual(values = c("skyblue", "orchid"), 
                    labels = c("Depresyon Yok", "Depresyon Var")) +
  
  labs(
    title = "Calısma Saatlerine Gore Depresyon Dagilimi",
    x = "Calisma Grubu",
    y = "Kisi Sayisi ",
    fill = "Durum",)


#soru4
#dereceler ile akademik baskı arasında anlamlı bir istatistiksel ilişki var mı?
kruskal_sonuc <- kruskal.test(akademik_baski ~ derece, data = ist347_odev_41)
print(kruskal_sonuc)
  #grafik
   # Akademik Baskı – Eğitim Derecesi Kutu Grafiği
  ggplot(data = ist347_odev_41,
         aes(x = derece, y = akademik_baski)) +
    
    geom_boxplot(fill = "darkcyan", color = "black") +
    
    labs(
      title = "Egitim Derecesine Gore Akademik Baski Duzeyleri",
      x = "Egitim Derecesi",
      y = "Akademik Baski Puani"
    ) +
    
    coord_flip() 
    
   
  
#soru5
# Ortalama notlar ile depresyon olup olmaması arasındaki anlamlı bir fark var mı
#mann whitney testi
wilcox_sonuc <- wilcox.test(ortalama_not ~ depresyon_durumu,  data = ist347_odev_41,exact = FALSE)
print(wilcox_sonuc)

# 2. Medyanları Karşılaştıralım 
tapply(ist347_odev_41$ortalama_not, 
       ist347_odev_41$depresyon_durumu, 
       median, 
       na.rm = TRUE)
print(wilcox_sonuc)
tapply(ist347_odev_41$ortalama_not, ist347_odev_41$depresyon_durumu, median, na.rm=TRUE)
#grafiğini çizelim
ggplot(ist347_odev_41, aes(x = ortalama_not, colour = factor(depresyon_durumu))) +
  geom_freqpoly(binwidth = 0.5) +
  facet_wrap(~akademik_baski)+
   labs(
    title = "Akademik Baski Duzeylerine Gore Not ve Depresyon Dagilimi",
    x = "Genel Not Ortalaması",
    y = "Ogrenci Sayisi",
    colour = "Durum" 
  ) 
  


#soru6
# Depresyonu olan ve olmayanlar arasında finansal stres puan farkı
library(dplyr)
#Hangi grup daha stresli?
ist347_odev_41 %>%
  group_by(depresyon_durumu) %>%
  summarise(
    kisi_sayisi = n(),
    medyan_stres = median(finansal_stres, na.rm = TRUE),
    ortalama_stres = mean(finansal_stres, na.rm = TRUE),
  ) %>%
  print()
# Wilcoxon Testi 
test_sonucu <- wilcox.test(finansal_stres ~ depresyon_durumu, 
                           data = ist347_odev_41, 
                           conf.int = TRUE)#güven aralığı
print(test_sonucu)

#grafik

grafik_verisi <- ist347_odev_41 %>%
  mutate(
    depresyon_etiket = factor(depresyon_durumu, levels = c(0, 1), labels = c("Yok", "Var")),
    finansal_stres_faktor = factor(finansal_stres)
  )
# Bar Grafiğini Çizdir
ggplot(grafik_verisi, aes(x = finansal_stres_faktor, fill = depresyon_etiket)) +
  geom_bar(position = "dodge") + # Yan yana sütunlar için "dodge", üst üste için "stack"
  scale_fill_manual(values = c("Yok" = "#7BC043", "Var" = "gold1")) +
  labs(
    title = "Finansal Stres Düzeylerine Göre Depresyon Sayıları",
    x = "Finansal Stres Puanı (1: Düşük, 5: Yüksek)",
    y = "Öğrenci Sayısı",
    fill = "Depresyon"
  ) 



 #soru7
 #ailedeki mental hastalık geçmişi ve depresyon arasındaki ilişki
# 1. Çapraz Tablo
# Satır:Ailede mental hastalık geçmişiSütun :Depresyon Durumu
tablo <- table(ist347_odev_41$ailede_mental_hastalik_gecmisi, ist347_odev_41$depresyon_durumu)

print("Veri Tablosu:")
print(tablo)

# 2. Test ve Risk Hesabı
test_sonucu <- fisher.test(tablo)

print(test_sonucu)
# Odds Ratio (Ne kadar artırıyor?) - Fisher testi OR değerini verir
fisher.test(tablo)$estimate
  # grafk
  ggplot(ist347_odev_41, aes(x = ailede_mental_hastalik_gecmisi, 
                             fill = depresyon_durumu)) +
    geom_bar(position = "dodge") +
      # Renkler
    scale_fill_manual(
      values = c("0" = "tomato1", "1" = "seagreen"),
      labels = c("0" = "Depresyon Yok", "1" = "Depresyon Var")
    ) +
   # Etiketler
    labs(
      title = "Aile Mental Hastalık Gecmisine Gore Depresyon Durumu",
      x = "Ailede Mental Hastalık Gecmisi",
      y = "Kisi Sayisi",
      fill = "Depresyon Durumu"
    )


  
#soru8  
#diyet alışkanlıkarının depresyona etkisi
tablo_diyet <- table(ist347_odev_41$diyet_aliskanlik, ist347_odev_41$depresyon_durumu)
kikare_test <- chisq.test(tablo_diyet)

kikare_test <- chisq.test(tablo_diyet)
print(kikare_test)

tablo <- table(ist347_odev_41$diyet_aliskanlik, ist347_odev_41$depresyon_durumu)
print(tablo)

kikare_test$residuals
#grafik
diyet_verisi <- read_excel("ist347_odev_41.xlsx")
# Faktör dönüşümlerini yapalım 
diyet_verisi$depresyon_durumu <- factor(diyet_verisi$depresyon_durumu, 
                                        levels = c(0, 1), 
                                        labels = c("Depresyon Yok", "Depresyon Var"))

ggplot(diyet_verisi, aes(x = diyet_aliskanlik, fill = depresyon_durumu)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#2a9d8f", "#e76f51")) +
  labs(title = "Diyet ve Depresyon İliskisi",
       x = "Diyet Alıskanlıgı",
       y = "Yüzde (%)",
       fill = "Durum") +
  theme_minimal()  

#soru9
#ilaç kullananların uyku durumu arasında gruplar arası fark var mı ?
ist347_odev_41$uyku_durumu <- gsub("'", "", ist347_odev_41$uyku_durumu)
sirali_duzen <- c("Less than 5 hours", 
                  "5-6 hours", 
                  "7-8 hours", 
                  "More than 8 hours")
# 2. Değişken bu sıralamaya göre 'factor' olarak güncellenir.
ist347_odev_41$uyku_durumu <- factor(ist347_odev_41$uyku_durumu, 
                                     levels = sirali_duzen)
# 3. Tablo
tablo_ilac_uyku <- table(ist347_odev_41$uyku_durumu, ist347_odev_41$sakinlestirici_ilac)
print(tablo_ilac_uyku)
#ki-kare testi
tablo_ilac_uyku <- table(ist347_odev_41$uyku_durumu, ist347_odev_41$sakinlestirici_ilac)
chisq.test(tablo_ilac_uyku)


# Gerekli kütüphaneleri yükleyin

# 3. Grafik
ggplot(ist347_odev_41, aes(x = uyku_durumu, y = gunluk_calisma_saati, fill = sakinlestirici_ilac)) +
  geom_boxplot() +  # Dağılımı görmek için kutu grafiği
  labs(title = "Uyku Suresi ve Sakinlestirici Kullanımına Gore Calısma Saatleri",
       x = "Uyku Suresi",
       y = "Gunluk Çalısma Saati",
       fill = "Sakinlestirici İlac") 




#soru10
# Ortalamalar
ist347_odev_41 %>%
  group_by(diyet_aliskanlik) %>%
  summarise(
    Sayi = n(),
    Ortalama_Stres = mean(finansal_stres, na.rm = TRUE)
  ) %>%   
  print()

# Kruskal-Wallis Testi

kruskal.test(finansal_stres ~ diyet_aliskanlik, data = ist347_odev_41)

#grafik 

# Diyet alışkanlığını faktör olarak düzenleyelim
diyet_verisi$diyet_aliskanlik <- factor(diyet_verisi$diyet_aliskanlik, 
                                        levels = c("Healthy", "Moderate", "Unhealthy"))

#  Kutu Grafiği (Boxplot) + Nokta Grafiği (Jitter)
ggplot(diyet_verisi, aes(x = diyet_aliskanlik, y = finansal_stres, fill = diyet_aliskanlik)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) + 
  scale_fill_manual(values = c("#2a9d8f", "#e9c46a", "#e76f51")) +
  labs(title = "Diyet Gruplarına Göre Finansal Stres",
       subtitle = ,
       x = "Diyet Alışkanlığı",
       y = "Finansal Stres Puanı") +
  theme_minimal()


#soru11
#mann-whitney testi
#  Cinsiyete göre baskı farkı 
wilcox.test(akademik_baski ~ cinsiyet, data = ist347_odev_41)
#cinsiyetlerden hangisi daha çok akademik baskı hissediyor?
ist347_odev_41 %>%
  group_by(cinsiyet) %>%
  summarise(
    Sayi = n(),
    Ortalama_baski = mean(akademik_baski, na.rm = TRUE)
  ) %>%   
  print()
#grafik
#  Keman + Kutu Grafiği
ggplot(diyet_verisi, aes(x = cinsiyet, y = akademik_baski, fill = cinsiyet)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.15, color = "black", outlier.color = "red", alpha = 0.7) +
  scale_fill_manual(values = c("#f8ad9d", "#95d5b2")) + # Pembe ve Yeşil tonları
  labs(title = "Cinsiyete Göre Akademik Baskı Dağılımı",
       subtitle = ,
       x = "Cinsiyet",
       y = "Akademik Baskı Puanı (1-5)") 







#soru12
#spearman korelasyonu
# 2. Baskı ile notlar arasındaki ilişki 
cor.test(ist347_odev_41$akademik_baski, ist347_odev_41$ortalama_not, method = "spearman",exact=FALSE)
# Görselleştirme: Saçılım Grafiği (Scatter Plot) + Regresyon Çizgisi
ggplot(ist347_odev_41, aes(x = akademik_baski, y = ortalama_not)) +
  geom_jitter(alpha = 0.4, color = "#457b9d", width = 0.2) + 
  geom_smooth(method = "lm", color = "#e63946", se = TRUE) + 
  labs(title = "Akademik Baskı ve Ortalama Not İlişkisi",
       x = "Akademik Baskı Puanı (1-5)",
       y = "Ortalama Not (GPA)") 

#SORU13
# Kruskal-Wallis Testi (Uyku gruplarına göre baskı seviyeleri)
ist347_odev_41$uyku_durumu <- gsub("'", "", ist347_odev_41$uyku_durumu)
sirali_duzen <- c("Less than 5 hours", 
                  "5-6 hours", 
                  "7-8 hours", 
                  "More than 8 hours")
ist347_odev_41$uyku_durumu <- factor(ist347_odev_41$uyku_durumu, 
                                     levels = sirali_duzen)
ist347_odev_41 %>%
  group_by(uyku_durumu) %>%
  summarise(
    kisi_sayisi = n(),
    medyan_stres = median(akademik_baski, na.rm = TRUE),
    ortalama_stres = mean(akademik_baski, na.rm = TRUE),
  ) %>%
  print()
kruskal.test(akademik_baski ~ uyku_durumu, data = ist347_odev_41)

# 3. Grafik (Akademik Baskı vs Uyku Süresi)
ggplot(ist347_odev_41, aes(x = uyku_durumu, y = akademik_baski, fill = uyku_durumu)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Uyku Suresine Gore Akademik Baski Seviyesi",
       x = "Uyku Suresi",
       y = "Akademik Baski (1-5)",
       fill = "Uyku Grubu") 
 
#soru14 
#Yüksek akademik baskıya rağmen depresyon düzeyi düşük olanların ortak özellikleri
# Baskı puanı 4 ve üzeri olup depresyonu olmayanları filtreliyoruz
direncli_grup <- subset(ist347_odev_41, akademik_baski >= 4 & depresyon_durumu == 0)

#gerekli kütüphaneler
library(dplyr)
library(tidyr)
library(ggplot2)

# Önce dirençli grubu tanımla 
direncli_grup <- subset(ist347_odev_41, akademik_baski >= 4 & depresyon_durumu == 0)

#dirençleri grubun araştırmak istediğimiz değişkenlerini seçiyoruz.
direncli_grup %>%
  select(uyku_durumu, diyet_aliskanlik, ogrenci_tatmini, finansal_stres, ailede_mental_hastalik_gecmisi, cinsiyet) %>% 
  #formatı düzenle 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(cols = everything(), names_to = "Faktor", values_to = "Deger") %>% 
  
  # Grafiği çiz
  ggplot(aes(x = Deger)) +
  geom_bar(fill = "#2E8B57", color = "white") +
  facet_wrap(~Faktor, scales = "free", ncol = 3) + 
  
  labs(x = NULL, y = "Kişi Sayısı", 
       title = "Yüksek Baskıya Rağmen Depresyona Girmeyenlerin Özellikleri")








#soru15
library(dplyr) 
# 1. Mutate ve case_when ile Yaş Gruplarını Oluşturma
ist347_odev_41 <- ist347_odev_41 %>%
  mutate(yas_gruplari = case_when(
    yas <= 22 ~ "genc",               # 22 ve altı
    yas >= 23 & yas <= 26 ~ "orta_genc", # 23 ile 26 arası
    yas >= 27 ~ "yetiskin"            # 27 ve üzeri
  ))

# 2. count fonksiyonu ile grupları kontrol edelim
ist347_odev_41 %>%
  count(yas_gruplari)

# 3. Ki-Kare Testi 
# Oluşturduğumuz yeni 'yas_gruplari' sütunu ile depresyonu karşılaştırıyoruz
tablo_yas <- table(ist347_odev_41$yas_gruplari, ist347_odev_41$depresyon_durumu)

print(tablo_yas) # Tablo
kikare_sonuc <- chisq.test(tablo_yas)# test
print(kikare_sonuc)
kikare_sonuc$residuals   
# Grupların medyan yaşlarını hesapla 
mu <- ist347_odev_41 %>%
  group_by(depresyon_durumu) %>%
  summarise(grp_medyan = median(yas))

#Yoğunluk Grafiği
ggplot(ist347_odev_41, aes(x = yas, fill =as.factor(depresyon_durumu) )) +
  geom_density(alpha = 0.4) + 
  geom_vline(data = mu, aes(xintercept = grp_medyan, color =as.factor(depresyon_durumu) ),
             linetype = "dashed", size = 1) +
  labs(title = "Depresyon Durumuna Gore Yaş Dağılımı",
       x = "Yaş",
       y = "Yoğunluk",
       fill = "Depresyon",
       color = "Medyan") +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  scale_color_manual(values = c("#2980b9", "#c0392b")) 


#sayısal değişkenler  için özetleyici grafikler

num_for_classif <- ist347_odev_41 %>%  #özetini istediğimiz numerik değişkenler
  select(
    yas,
    gunluk_calisma_saati,
    finansal_stres,
    ortalama_not,
   akademik_baski,
   ogrenci_tatmini,
   depresyon_durumu
  ) %>%
  drop_na()

num_for_classif$depresyon_durumu <-
  as.factor(num_for_classif$depresyon_durumu)

GGally::ggpairs(
  num_for_classif,
  aes(color = depresyon_durumu, alpha = 0.7),
  columns = 1:6  #6 sütuna bölünecek
)+
 theme(axis.text.x = element_text(angle = 45, hjust = 1))




#ilişki matrisi

# Eğer 'depresyon_durumu'nu geçici olarak sayıya çeviriyoruz.
cor_data <- num_for_classif %>%
  mutate(depresyon_durumu = as.numeric(as.character(depresyon_durumu))) %>% 
  select(where(is.numeric)) # Sadece sayısal sütunları seç

# 2. Korelasyon Matrisini Hesaplama
cor_mat <- cor(cor_data, use = "pairwise.complete.obs")

# 3. Veriyi ggplot formatına çevirme
cor_long <- melt(cor_mat)

# 4. Grafik
ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") + # Kutucukların arasına beyaz çizgi
  geom_text(aes(label = round(value, 2)), size = 4) + # Korelasyon sayılarını yaz
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Korelasyon") +
  theme_minimal() + # Arka planı temizle
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(title = "Değişkenler Arası Korelasyon Matrisi")


