Czy uważa Pan(i), że życie jest pasjonujące?
================

Wstep
-----

W niniejszym tekście przeprowadzimy analizę, która odpowie nam na kilka pytań dotyczących `pasjonującego życia` w latach 90 w Polsce. Analiza ma zadanie przedstawienie kilku bardzo pomocnych bibliotek takich jak `caret`, `foreign`, `randomForest`, `dplyr` oraz metod często wykorzystywanych w analizie danych czyli `regresji logitwej` czy `Random Forest`. Dane pochodzą z archiwum [diagnozy społeczenej](http://www.diagnoza.com/) i obejmują lata 91-97. Przejdźmy więc do analizy.

Pobranie danych
---------------

Dane zostały ręczne ściągnięte na dysk. Oczywiście można było zrobić to za pomocą R, ale to jest temat na inna analizę. Dane są w formacie Staty czyli `.dta` i odczytujemy je za pomocą biblioteki `foregin`.

``` r
library(foreign)
library(dplyr)
diagnoza = read.dta(adres)
```

Na początku rzućmy okiem na dane. Jak widać poniżej dane są w formacie `Data Frame` z 8910 obserwacjami i 149 kolumnami. Nie wszystkie z nich będą potrzebne do analizy więc przeprowadzimy wstępną selekcje obserwacji.

``` r
class(diagnoza)
```

    ## [1] "data.frame"

``` r
dim(diagnoza)
```

    ## [1] 8910  149

Na początku wybierzmy rok obserwacji. W diagnozie umieszczono dane z okresu 91-97 i dla przejrzystości wyników wybierzmy okres, który ma najwięcej obserwacji.

``` r
diagnoza %>%
    group_by(pgssyear) %>%
    count()
```

    ## # A tibble: 5 × 2
    ##   pgssyear     n
    ##     <fctr> <int>
    ## 1    1992r  1647
    ## 2    1993r  1649
    ## 3    1994r  1609
    ## 4    1995r  1603
    ## 5 1997/98r  2402

Jak widać najwięcej obserwacji znajduje się w 97 roku, dlatego analiza będzie dotyczyć tego okresu. Następnie wybierzmy zmienne, które będą podlegać analizie. Będą nimi:

-   life - główna zmienna odpowiadająca na pytanie czy respondent uważa swoje życie za pasjonujące.

-   region - region z który pochodzi respondent. Nie są to województwa, w tamtym okresie obowiązywał jeszcze podział na 49 województw. Zmienna ta przyjmuje 8 wartości.

-   size - wielkość miejscowości, którą zamieszkuje respondent.

-   hompop - ilość osób w gospodarstwie w której mieszka respondent

-   sex - płeć respondenta

-   age - wiek respondenta

-   siops - subiektywna ocena prestiżu wykonywanego zawodu przez respondenta

-   rincome - dochód respondenta

-   marital - stan cywilny respondenta

-   childs - liczba dzieci respondenta

-   finalter - subiektywna opinia respondenta odnośnie dzisiejszej sytuacji finansowej w porównaniu z latami ubiegłymi

``` r
diag97 <- diagnoza %>%
    filter(pgssyear == "1997/98r", as.integer(life) < 4) %>%
    select(life, region8, size, hompop, sex, age,
           siops, rincome, marital, childs, finalter)
```

Przeprowadźmy wstępna analizę danych wykorzystując podstawowe funkcje pakietu `dplyr` Jak widać region pomorski osiągnął średnio najwyższy poziom w skali pasjonującego życia. najgorzej w tym rankingu wypadł region północno-wschodni.

``` r
diag97 %>%
    group_by(region8) %>%
    summarise(pasja = mean(as.numeric(life))) %>%
    arrange(pasja)
```

    ## # A tibble: 8 × 2
    ##               region8    pasja
    ##                <fctr>    <dbl>
    ## 1            pomorski 1.709544
    ## 2              śląski 1.720812
    ## 3            zachodni 1.729483
    ## 4            wschodni 1.767442
    ## 5        wielkopolski 1.792517
    ## 6           centralny 1.803191
    ## 7          małopolski 1.813953
    ## 8 północno - wschodni 1.832298

Następnie rzućmy okiem na stan cywilny respondentów. Według danych z 97r bycie kawalerem czy panną wiązało się z wyższym poziom pozytywnych opinii dotyczących życia. Jednak należny pamiętać, że ta obserwacja nie świadczy o tym, że kawalerzy są szczęśliwsi niż żonaci mężczyźni, Duża wpływ na ten wynik mogą mieć inne zmienne np. wiek który silnie koreluje z stanem cywilnym.

``` r
diag97 %>%
    group_by(marital) %>%
    na.omit(marital) %>%
    filter(n() > 20) %>%
    summarise(pasja = mean(as.numeric(life))) %>%
    arrange(pasja)
```

    ## # A tibble: 4 × 2
    ##       marital    pasja
    ##        <fctr>    <dbl>
    ## 1 kaw / panna 1.647959
    ## 2    rozwiedz 1.666667
    ## 3 małż / konk 1.728288
    ## 4       wdowi 1.837838

Ostatnią zmienną grupująca jest ilość dzieci. W tym przypadku odrzuciliśmy obserwacje, który suma obserwacji wynosiła mniej niż 20.

``` r
diag97 %>%
    group_by(childs) %>%
    na.omit(childs) %>%
    filter(n() > 20) %>%
    summarise(pasja = mean(as.numeric(life))) %>%
    arrange(pasja)
```

    ## # A tibble: 5 × 2
    ##            childs    pasja
    ##            <fctr>    <dbl>
    ## 1 nie miał dzieci 1.664032
    ## 2           dwoje 1.690289
    ## 3           jedno 1.707317
    ## 4           troje 1.798883
    ## 5          czworo 1.808511

Jak widać osoby bezdzietne osiągnęły tutaj najwyższy wynik. Jednak tutaj występuję podobna zależność co poprzednio. W celu weryfikacji hipotezy, że wyższa ilość dzieci negatywnie wpływa na ocenę życia trzeba by było przeprowadzić analizę, która kontroluje inne zmienne (jak np. wiek)

Przygotowanie danych.
=====================

Zanim przejdziemy do regresji, należy najpierw dane odpowiednio przygotować. Na szczęście dane pochodzące z `Diagnozy Społecznej` są profesjonalnie przygotowane i dla naszych potrzeb wystarczy ich niewielka obróbka.

W większości pytań odpowiedzi przyjmują kilka poziomów. Niestety czasem pojawiają się odpowiedzi typu: `nie wiem`, `nie mam zdania` itp. Dla większości analiz takie odpowiedzi są mało przydatne i tak jest również w tym przypadku. Dlatego należy je usunąć. Rzućmy okiem jakie odpowiedzi przyjmują poszczególne pytania. Jak widać poniżej zmienne `life` oraz `finalter` przyjmują niechciane odpowiedzi `nie wiem`. Usuwamy te obserwacji z naszej analizy.

``` r
sapply(diag97, levels)
```

    ## $life
    ## [1] "pasjonujące"    "zwyczajne"      "nudne"          "nie mam zdania"
    ## [5] "brak danych"   
    ## 
    ## $region8
    ## [1] "centralny"           "wielkopolski"        "śląski"             
    ## [4] "zachodni"            "pomorski"            "północno - wschodni"
    ## [7] "wschodni"            "małopolski"         
    ## 
    ## $size
    ## [1] "wieś"         "m do 10tys"   "m 10-24tys"   "m 25-49tys"  
    ## [5] "m 50-99tys"   "m 100-249tys" "m 250-499tys" "m 500 + tys" 
    ## 
    ## $hompop
    ##  [1] "jedna (resp)"    "dwie osoby"      "trzy osoby"     
    ##  [4] "cztery osoby"    "piec osób"       "sześć osób"     
    ##  [7] "siedem osób"     "osiem osób"      "dziewięć osób"  
    ## [10] "dziesięć osób"   "jedenaście osób" "dwanaście osób" 
    ## 
    ## $sex
    ## [1] "mężczyzna" "kobieta"  
    ## 
    ## $age
    ## NULL
    ## 
    ## $siops
    ## NULL
    ## 
    ## $rincome
    ## NULL
    ## 
    ## $marital
    ## [1] "małż / konk" "wdowi"       "rozwiedz"    "separacja"   "kaw / panna"
    ## [6] "brak danych"
    ## 
    ## $childs
    ##  [1] "nie miał dzieci"    "jedno"              "dwoje"             
    ##  [4] "troje"              "czworo"             "pięcioro"          
    ##  [7] "sześcioro"          "siedmioro"          "ośmioro lub więcej"
    ## [10] "brak danych"       
    ## 
    ## $finalter
    ## [1] "poprawiała się"        "pogarszała się"        "pozostawała taka sama"
    ## [4] "nie wiem"              "brak danych"

``` r
diag97 <- diag97 %>%
    filter(as.integer(life) < 4, as.integer(finalter) < 4)
```

Następnie zmienimy typ danych z `factor` na `integer` w kilku zmiennych. Nie jest to konieczne, jednak wygodniej będzie przedstawić wyniki analiz.

``` r
int = c("size", "hompop", 'sex')

change_integer <- function(x) {
    x <- as.integer(x)
}

diag97[int] <- sapply(diag97[int], change_integer)
```

Przejmy teraz do sposobu kodowania zmiennych. Główna zmienna przyjmuje obecnie 3 odpowiedzi: `pasjonujące`, `zwyczajne`, `nudne`. Dla potrzeb regresji logitowej przekształćmy ją na zmienną binarną, gdzie 0 to odpowiedzi `zwyczajne` oraz `nudne` a 1 to odpowiedzi `pasjonujące`.

``` r
diag97['life_int'] <- as.integer(diag97$life)

diag97['life_int'] <- cut(diag97$life_int, breaks = c(0,1,3), labels = c(1,0))

diag97 %>% group_by(life_int) %>%
    count()
```

    ## # A tibble: 2 × 2
    ##   life_int     n
    ##     <fctr> <int>
    ## 1        1   712
    ## 2        0  1605

W zmiennej `finalter` występuje taki problem iż przyjmuje ona poziomy `poprawiła się`, `pogorszyła sie` oraz `pozostała taka sama` w takiej samej kolejności. Dla potrzeb analizy powinniśmy zmienić kolejność poziomów, by odpowiedź `pogarszała się` była stanem gorszym niż odpowiedź `pozostawała taka sama`. Na koniec zamienimy typ zmiennej na `integer`

``` r
levels(diag97$finalter) <- c("pogarszała się","pozostawała taka sama", "poprawiała się", "nie wiem","brak danych")
diag97$finalter <- as.integer(diag97$finalter)
```

Ostatnią modyfikacją w tej części będzie zmiana zmiennej informującej o statusie cywilnym. Przekształćmy ja w taki sposób iż 1 to osoby w związku małżeńskim a 0 pozostałe inne.

``` r
diag97['marital_bin'] <- as.integer(diag97$marital)
diag97$marital_bin <- cut(diag97$marital_bin,
                          breaks = c(0,1,5),
                          labels = c(1,0))
diag97 <- diag97 %>% 
                na.omit(marital_bin) 
```

Modelowanie
===========

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

Teraz przejdziemy do głównej części analizy. Na tym etapie wykonamy regresje `logitową` a następnie klasyfikacje metodą `Random Forest`. Do tego będą potrzebnie dwie biblioteki: `randomForest` oraz `caret`.

Dużą zaletą pakietu `caret` jest możliwość zautomatyzowania wielu procedur jak no. tworzenie `Confusion Matrix` czy przeprowadzenia wielokrotnie powtórzonego testu `Cross-Valididation`. W niniejszej regresji logitowej do weryfikacji modelu przeprowadzimy 5 krotnie powtórzoną `10 Fold Cross Valididation`

``` r
logisticReg <- train(life_int ~ sex + age + hompop + size + marital_bin + siops + finalter,
                     data = diag97,
                     method = 'glm',
                     trControl = trainControl(method = 'repeatedcv',
                                              repeats = 5))
```

``` r
summary(logisticReg)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1367  -1.2257   0.7192   0.8895   1.5539  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   1.385715   0.511307   2.710 0.006725 ** 
    ## sex          -0.020969   0.134757  -0.156 0.876341    
    ## age           0.018111   0.006987   2.592 0.009541 ** 
    ## hompop       -0.054536   0.044189  -1.234 0.217144    
    ## size         -0.094259   0.028671  -3.288 0.001011 ** 
    ## marital_bin0 -0.116342   0.163087  -0.713 0.475614    
    ## siops        -0.033161   0.006008  -5.520  3.4e-08 ***
    ## finalter      0.299460   0.081077   3.694 0.000221 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1388.5  on 1089  degrees of freedom
    ## Residual deviance: 1312.3  on 1082  degrees of freedom
    ## AIC: 1328.3
    ## 
    ## Number of Fisher Scoring iterations: 4

Jak widać powyżej, zmiennymi istotnymi okazały się wiek, wielkość miejscowości, ocena prestiżu wykonywanego zawodu i subiektywna ocena obecnej sytuacji w porównaniu do lat poprzednich. Ciekawe jest to, iż im wyższa ocena prestiżu wykonywanego zawodu, tym niższa ocena życia. Z wiekiem ludzie uważają, że życie jest bardziej pasjonujące. Im większa miejscowość tym niższa ocena życia jako pasjonującego doświadczenia. Warto jednak zauważyć, że największy wpływ ma ocena własnej sytuacji w porównaniu do lat ubiegłych.

``` r
logisticReg
```

    ## Generalized Linear Model 
    ## 
    ## 1090 samples
    ##    7 predictor
    ##    2 classes: '1', '0' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 5 times) 
    ## Summary of sample sizes: 980, 982, 981, 982, 980, 982, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.6780276  0.1321137
    ## 
    ## 

Przeprowadzony model cechuje jedynie 67,8% precyzji. Jest to bardzo zły wynik ponieważ, wykorzystując najprostszą regułę przydzielania każdej obserwacji do wartości 0, otrzymalibyśmy 726/(726+364) = 66,6% precyzji, czyli niewiele mniej niż przy bardziej skomplikowanej regresji logitowej.

Klasyfikacja
============

Po regresji wykorzystamy klasyfikację metodą `Random Forest`. Jest to jedna z najczęściej wykorzystywanych metod analiz danych. Do przeprowadzenia niniejszej operacji wykorzystamy pakiet `randomForest`.

``` r
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

Dodatkową zamiast wykorzystując funkcje pakietu `caret` przeprowadzimy `ręcznie` testowanie modelu, za pomocą próbki trenującej (75% obserwacji) oraz próbki testującej (25% obserwacji)

``` r
set.seed(123)
smp_size <- floor(0.75* nrow(diag97))

train_ind <- sample(seq_len(nrow(diag97)), size = smp_size)

train <- diag97[train_ind, ]
test  <- diag97[-train_ind, ]

RFmodel <- randomForest(life_int ~ sex + age + hompop + size + marital_bin + siops + finalter,
                        data = train, ntree = 5)
```

Po wytrenowaniu modelu możemy przejść do jego weryfikacji. Tutaj również nam się przyda pakiet `caret`, który automatyzuje proces budowy `confusion matrix`. Jak widać poniżej model `Random Forest` osiągnąć gorszy wynik niż `regresja logitowa` przy precyzji jedynie 58%.

``` r
test$class <- predict(RFmodel, test)

confusionMatrix(data = test$class,
            reference = test$life_int,
            positive = '1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   1   0
    ##          1  24  35
    ##          0  67 147
    ##                                          
    ##                Accuracy : 0.6264         
    ##                  95% CI : (0.566, 0.6839)
    ##     No Information Rate : 0.6667         
    ##     P-Value [Acc > NIR] : 0.929066       
    ##                                          
    ##                   Kappa : 0.0783         
    ##  Mcnemar's Test P-Value : 0.002144       
    ##                                          
    ##             Sensitivity : 0.26374        
    ##             Specificity : 0.80769        
    ##          Pos Pred Value : 0.40678        
    ##          Neg Pred Value : 0.68692        
    ##              Prevalence : 0.33333        
    ##          Detection Rate : 0.08791        
    ##    Detection Prevalence : 0.21612        
    ##       Balanced Accuracy : 0.53571        
    ##                                          
    ##        'Positive' Class : 1              
    ## 

Podsumowanie
============

W dokumencie przedstawiono dwa sposoby analizy danych często spotykanych w `data minig`, mianowicie `regresje logitową` oraz `klasyfikację`. Chociaż oba modele prezentują bardzo niską wartość predykcyjną to jednak głównym zadaniem niniejszego pliku było przedstawienie mozaikowości takich bibliotek jak `caret` czy `randomForest` i ich wykorzystania w praktyce.
