Leinenger\_2018\_JEP:LMC
================
Mallorie Leinenger
June 28, 2018

Leinenger M. (in press) Survival analyses reveal how early phonological processing affects eye movements during reading. *Journal of Experimental Psychology: Learning, Memory, and Cognition*
==============================================================================================================================================================================================

Overview
--------

These data come from four experiments investigating the time course of phonological coding during reading. Participants read single-sentence stimuli (see example stimuli below) while their eye movement behavior was recorded. Sentences contained either a correct target word that was moderately predictable in context (e.g., *beach*, *sleep*), a contextually inappropriate (non-)word that shared phonology with the correct target word (e.g., *beech*, *sleap*), or a contextually inappropriate (non-)word that was match on orthographic overlap, but did not share phonology (e.g., *bench*, *slerp*).

-   The surfers traveled to the world-famous *beach/beech/bench* where the waves were very large. (Exps. 1 & 3)
-   Because of his insomnia, Caleb couldn’t *sleep/sleap/slerp* even though he was tired. (Exps. 2 & 4)

In Experiments 1 & 2 participants directly fixated each type of target word, and in Experiments 3 & 4 the phonologically related and orthographic control (non-)words were presented as parafoveal previews using the invisible boundary display-change paradigm (Rayner, 1975). In addition to means analyses, survival analyses were conducted (following the method outlined in Reingold & Sheridan, 2014) to determine the earliest observable influence of phonological coding on the eye movement record.

Abstract
--------

Numerous studies have provided evidence that readers generate phonological codes while reading. However, a central question in much of this research has been how early these codes are generated. Answering this question has implications for the roles that phonological coding might play for skilled readers, especially whether phonological codes affect the identification of most words, which can only be the case if these codes are generated rapidly. To investigate the time course of phonological coding during silent reading, the present series of experiments examined survival analyses of first-fixation durations on phonologically related (homophones, pseudohomophones) and orthographic control (orthographically matched words and non-words) stimuli that were either embedded in sentences in place of correct targets (Experiments 1 and 2) or presented as parafoveal previews for correct targets using the boundary paradigm (Experiments 3 and 4). Survival analyses revealed a discernible difference between processing the phonologically related versus the orthographic control items by as early as 160 ms from the start of fixation on average (160–173 ms across experiments). Because only approximately 18% of first fixation durations were shorter than these mean estimates and follow-up tests revealed that earlier divergence point estimates were associated with shorter gaze durations (e.g., more rapid word identification), results suggest that skilled readers rapidly generate phonological codes during normal, silent reading and that these codes may affect the identification of most words.

Experiment 1
------------

Data for 48 participants (**subject**) reading 168 sentences (**item**) that either contained the correct target word, a homophone, or an orthographic control word.

-   The surfers traveled to the world-famous *beach/beech/bench* where the waves were very large.

**ffd** = first fixation duration, **sfd** = single fixation duration, **gzd** = gaze duration, **gpt** = go-past time, **tvt** = total time, **skp** = fixation probability (inverse of skipping), **rgi** = regression-in probability, **rgo** = regression-out probability.

    ## 'data.frame':    7698 obs. of  12 variables:
    ##  $ subject  : Factor w/ 48 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ item     : Factor w/ 168 levels "1","2","3","4",..: 1 2 3 4 5 7 8 9 10 11 ...
    ##  $ condition: Factor w/ 3 levels "1","2","3": 1 2 3 1 2 1 2 3 1 2 ...
    ##  $ ffd      : int  265 158 NA NA 198 NA 230 NA NA NA ...
    ##  $ sfd      : int  265 158 NA NA 198 NA 230 NA NA NA ...
    ##  $ gzd      : int  265 158 NA NA 198 NA 230 NA NA NA ...
    ##  $ tvt      : int  265 158 NA 173 198 NA 230 NA NA NA ...
    ##  $ gpt      : int  265 158 NA NA 198 NA 230 NA NA NA ...
    ##  $ skp      : int  1 1 0 0 1 0 1 0 0 0 ...
    ##  $ rgi      : int  0 0 0 1 0 0 0 0 0 0 ...
    ##  $ rgo      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Condition: Factor w/ 3 levels "Control","Homophone",..: 3 2 1 3 2 3 2 1 3 2 ...

    ##   subject item condition ffd sfd gzd tvt gpt skp rgi rgo Condition
    ## 1       1    1         1 265 265 265 265 265   1   0   0    Target
    ## 2       1    2         2 158 158 158 158 158   1   0   0 Homophone
    ## 3       1    3         3  NA  NA  NA  NA  NA   0   0   0   Control
    ## 4       1    4         1  NA  NA  NA 173  NA   0   1   0    Target
    ## 5       1    5         2 198 198 198 198 198   1   0   0 Homophone
    ## 6       1    7         1  NA  NA  NA  NA  NA   0   0   0    Target

### Means & Standard Errors for Exp. 1

``` r
byCond <- group_by(data, Condition)
stats.m<-summarize_if(byCond,is.numeric,funs(mean), na.rm=TRUE)
stats.se<-summarize_if(byCond,is.numeric,funs(std.error), na.rm=TRUE)
stats.m
```

    ## # A tibble: 3 x 9
    ##   Condition   ffd   sfd   gzd   tvt   gpt   skp   rgi    rgo
    ##   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 Control    245.  249.  276.  436.  361. 0.671 0.361 0.119 
    ## 2 Homophone  241.  244.  266.  372.  325. 0.668 0.264 0.0998
    ## 3 Target     225.  226.  239.  272.  280. 0.662 0.121 0.0736

``` r
stats.se
```

    ## # A tibble: 3 x 9
    ##   Condition   ffd   sfd   gzd   tvt   gpt     skp     rgi     rgo
    ##   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Control    2.27  2.53  3.17  6.56  6.59 0.00929 0.00949 0.00639
    ## 2 Homophone  2.13  2.36  2.84  5.64  5.21 0.00932 0.00872 0.00593
    ## 3 Target     1.86  1.94  2.23  3.53  4.19 0.00931 0.00643 0.00514

### Figure of early measures for Exp. 1

Because it's easier to visualize on a figure than in a table, here are the earliest measures where you can see that readers spent the least amount of time fixating (i.e., processing) the correct target, the most amount of time fixating the orthographic control word, and an intermediate amount of time fixating the homophone--demonstrating an advantage for processing a phonologically related word over a word that shares the same number of letters (i.e., is physically similar).

![](Leinenger_2018_files/figure-markdown_github/descriptives%20plot%20exp1-1.png)

### Survival Analyses for Exp. 1

Although this graded processing suggests that readers activate phonological codes which help them more easily process words which are phonologically related to the expected/contextually appropriate target, just how rapidly these codes come online and begin to influence behavior is not clear. In order to better characterized the time course of code generation, survival analyses were conducted to determine the earliest observable influence of phonology on behavior.

In these analyses, the rate of "surviving" fixations is compared across two condition (here the phonologically related and orthographic control condition) to determine the point at which the survival curves diverge (e.g., the point in time at which more readers have left the phonologically related word than the orthographic control word) which is indicative of easier processing in one condition relative to the other. The divergence point then reflects the earliest observable influence of phonological coding on behavior.

For the paper, I used Matlab to calculate divergence point estimates, however here I am using the new [RTsurvival package](https://github.com/matsukik/RTsurvival)

``` r
tmp<-select(data,subject,ffd,condition) %>% arrange(condition) %>%
  rename(duration=ffd)
tmp$condition <- as.numeric(tmp$condition)
survdata<- as.data.frame(tmp %>% as_tibble() %>% mutate(condition = condition-1)) %>%
  filter(condition != 0, !is.na(duration))
survdata$condition <- as.factor(survdata$condition)
str(survdata)
```

    ## 'data.frame':    3425 obs. of  3 variables:
    ##  $ subject  : Factor w/ 48 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ duration : int  158 198 230 198 304 484 260 173 244 390 ...
    ##  $ condition: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...

We have 48 participants who each have between 35 and 100 data points:

``` r
n.per.sbj <- table(survdata$subject)
length(n.per.sbj)
```

    ## [1] 48

``` r
range(n.per.sbj)
```

    ## [1]  35 100

We can now use these data to generate divergence point estimates (DPE) for each participant:

``` r
ip.dpa <- DPA.ip(survdata$subject, survdata$duration, survdata$condition, quiet = TRUE)
dpe <- as.data.frame(ip.dpa$dp_matrix)
# critical columns in output
# 'dpcount' = the number of iterations (out of 1000) on which a DPE was obtained
#' median_dp_duration' = median of the DPEs obtained on each iteration
str(dpe)
```

    ## 'data.frame':    48 obs. of  6 variables:
    ##  $ subject        : Factor w/ 48 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dpcount        : num  1000 1000 743 939 802 1000 816 1000 1000 1000 ...
    ##  $ median_dp_point: num  45 1 621 209 1 109 998 1 1 1 ...
    ##  $ median_duration: num  162 136 202 196 142 ...
    ##  $ ci.lower       : num  161.5 136.5 92.5 179 141.5 ...
    ##  $ ci.upper       : num  162 194 204 240 249 ...

``` r
dpe$subject[dpe$dpcount<500]
```

    ## [1] 19 20 21 25 27 30 38
    ## 48 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 ... 48

Doing so reveals that the DPE for 7 participants were unreliable (i.e., a DP was found on fewer than half of the iterations). Removing those participants reveals a mean DPE of ~173 across the remaining participants (the value moves around ever so slightly each time the bootstrap re-sampling procedure runs). This tells us that on average, phonological coding was influencing behavior by as early as 173 ms after fixation on the target word began.

``` r
dpe.rel <- filter(dpe, dpcount >= 500)
summarize(dpe.rel, mean.dpe = mean(median_duration, na.rm=TRUE))
```

    ##   mean.dpe
    ## 1 172.7073

Finally, we can represent this visually by examining the survival curves created using the ggsurv function. **Note**--the values displayed on this figure are from the published version of the manuscript and might vary by ~ 1ms from those generated here.

``` r
data$survdat<-as.integer(ifelse(data$Condition=="Target",NA,data$ffd))
tmp2 <- filter(data, !subject %in% c(19, 20, 21, 25, 27, 30, 38))
ffd.surv <- survfit(Surv(survdat) ~ Condition, data=tmp2)
pl2<-ggsurv(s=ffd.surv)

pl2 + geom_vline(xintercept = 172.61, linetype = "dotted") + 
  annotate("rect", xmin=152.4, xmax=192.9, ymin=0, ymax=1, alpha = .2) + 
  annotate("text", x = 450, y = 0.75, label = "Divergence Point = 173ms", size = 5) +
  annotate("text", x = 450, y = 0.7, label = "95% CI: 152 - 193ms", size = 5) + 
  theme(axis.text.x = element_text(colour="grey4", size=16), axis.text.y = element_text(colour = "grey4", size=16)) + 
  labs(y = "Survival", x = "Time") + theme_grey(base_size=16)
```

![](Leinenger_2018_files/figure-markdown_github/survival%20figure%20Exp%201-1.png)

Experiment 2
------------

Data for 48 participants (**subject**) reading 180 sentences (**item**) that either contained the correct target word, a pseudohomophone (i.e., phonologically related non-word), or an orthographic control non-word.

-   Because of his insomnia, Caleb couldn’t *sleep/sleap/slerp* even though he was tired.

**ffd** = first fixation duration, **sfd** = single fixation duration, **gzd** = gaze duration, **gpt** = go-past time, **tvt** = total time, **skp** = fixation probability (inverse of skipping), **rgi** = regression-in probability, **rgo** = regression-out probability.

    ## 'data.frame':    8347 obs. of  12 variables:
    ##  $ subject  : Factor w/ 48 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ item     : Factor w/ 180 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ condition: Factor w/ 3 levels "1","2","3": 1 2 3 1 2 3 1 2 3 1 ...
    ##  $ ffd      : int  NA 186 218 214 178 NA NA NA 192 219 ...
    ##  $ sfd      : int  NA 186 218 214 NA NA NA NA 192 219 ...
    ##  $ gzd      : int  NA 186 218 214 434 NA NA NA 192 219 ...
    ##  $ tvt      : int  NA 186 218 214 434 286 525 379 666 219 ...
    ##  $ gpt      : int  NA 186 218 214 434 NA NA NA 192 219 ...
    ##  $ skp      : int  0 1 1 1 1 0 0 0 1 1 ...
    ##  $ rgi      : int  0 0 0 0 0 1 1 1 0 0 ...
    ##  $ rgo      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Condition: Factor w/ 3 levels "Control","Pseudohomophone",..: 3 2 1 3 2 1 3 2 1 3 ...

    ##   subject item condition ffd sfd gzd tvt gpt skp rgi rgo       Condition
    ## 1       1    1         1  NA  NA  NA  NA  NA   0   0   0          Target
    ## 2       1    2         2 186 186 186 186 186   1   0   0 Pseudohomophone
    ## 3       1    3         3 218 218 218 218 218   1   0   0         Control
    ## 4       1    4         1 214 214 214 214 214   1   0   0          Target
    ## 5       1    5         2 178  NA 434 434 434   1   0   0 Pseudohomophone
    ## 6       1    6         3  NA  NA  NA 286  NA   0   1   0         Control

### Means & Standard Errors for Exp. 2

``` r
byCond <- group_by(data, Condition)
stats.m <- summarize_if(byCond,is.numeric,funs(mean), na.rm=TRUE)
stats.se <- summarize_if(byCond,is.numeric,funs(std.error), na.rm=TRUE)
stats.m
```

    ## # A tibble: 3 x 9
    ##   Condition         ffd   sfd   gzd   tvt   gpt   skp    rgi    rgo
    ##   <fct>           <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
    ## 1 Control          260.  263.  300.  410.  343. 0.739 0.273  0.0732
    ## 2 Pseudohomophone  248.  251.  275.  325.  309. 0.745 0.166  0.0623
    ## 3 Target           218.  220.  232.  250.  254. 0.676 0.0773 0.0469

``` r
stats.se
```

    ## # A tibble: 3 x 9
    ##   Condition         ffd   sfd   gzd   tvt   gpt     skp     rgi     rgo
    ##   <fct>           <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Control          2.40  2.63  3.49  5.85  4.91 0.00836 0.00849 0.00496
    ## 2 Pseudohomophone  2.17  2.36  2.85  4.02  4.09 0.00825 0.00705 0.00457
    ## 3 Target           1.78  1.88  2.16  2.72  3.23 0.00885 0.00506 0.00400

### Figure of early measures for Exp. 2

As with Experiment 1, the graded processing is clearly visible, where readers have an advantage to processing a phonologically related word over a control word matched on visual similarity.

![](Leinenger_2018_files/figure-markdown_github/descriptives%20plot%20exp2-1.png)

### Survival Analyses for Exp. 2

Survival analyses were again conducted to determine just how rapidly phonological codes come online and begin to influence behavior, The DPE reflects the earliest observable influence of phonological coding on behavior.

``` r
tmp<-select(data,subject,ffd,condition) %>% arrange(condition) %>%
  rename(duration=ffd)
tmp$condition <- as.numeric(tmp$condition)
survdata<- as.data.frame(tmp %>% as_tibble() %>% mutate(condition = condition-1)) %>%
  filter(condition != 0, !is.na(duration))
survdata$condition <- as.factor(survdata$condition)
str(survdata)
```

    ## 'data.frame':    4120 obs. of  3 variables:
    ##  $ subject  : Factor w/ 48 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ duration : int  186 178 249 164 203 431 228 196 295 206 ...
    ##  $ condition: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...

We have 48 participants who each have between 55 and 110 data points:

``` r
n.per.sbj <- table(survdata$subject)
length(n.per.sbj)
```

    ## [1] 48

``` r
range(n.per.sbj)
```

    ## [1]  55 110

We can now use these data to generate divergence point estimates (DPE) for each participant:

``` r
ip.dpa <- DPA.ip(survdata$subject, survdata$duration, survdata$condition, quiet = TRUE)
dpe <- as.data.frame(ip.dpa$dp_matrix)
# critical columns in output
# 'dpcount' = the number of iterations (out of 1000) on which a DPE was obtained
#' median_dp_duration' = median of the DPEs obtained on each iteration
str(dpe)
```

    ## 'data.frame':    48 obs. of  6 variables:
    ##  $ subject        : Factor w/ 48 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dpcount        : num  1000 1000 1000 1000 997 984 0 1000 1000 1000 ...
    ##  $ median_dp_point: num  430 138 1 239 1 ...
    ##  $ median_duration: num  212 176 144 165 81 ...
    ##  $ ci.lower       : num  210.5 175 144.5 80.5 81 ...
    ##  $ ci.upper       : num  212 176 144 173 103 ...

``` r
dpe$subject[dpe$dpcount<500]
```

    ## [1] 7  16 26 45 46
    ## 48 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 ... 48

Doing so reveals that the DPE for 5 participants were unreliable (i.e., a DP was found on fewer than half of the iterations). Removing those participants reveals a mean DPE of ~161 across the remaining participants (the value moves around ever so slightly each time the bootstrap re-sampling procedure runs). This tells us that on average, phonological coding was influencing behavior by as early as 161 ms after fixation on the target word began.

``` r
dpe.rel <- filter(dpe, dpcount >= 500)
summarize(dpe.rel, mean.dpe = mean(median_duration, na.rm=TRUE))
```

    ##   mean.dpe
    ## 1 162.6279

Finally, we can represent this visually by examining the survival curves created using the ggsurv function. **Note**--the values displayed on this figure are from the published version of the manuscript and might vary by ~ 1ms from those generated here.

``` r
data$survdat<-as.integer(ifelse(data$Condition=="Target",NA,data$ffd))
tmp2 <- filter(data, !subject %in% c(7, 16, 26, 45, 46))
ffd.surv <- survfit(Surv(survdat) ~ Condition, data=tmp2)
pl2<-ggsurv(s=ffd.surv)

pl2 + geom_vline(xintercept = 161.06, linetype = "dotted") + 
  annotate("rect", xmin=146, xmax=176, ymin=0, ymax=1, alpha = .2) + 
  annotate("text", x = 450, y = 0.75, label = "Divergence Point = 161ms", size = 5) +
  annotate("text", x = 450, y = 0.7, label = "95% CI: 146 - 176ms", size = 5) + 
  theme(axis.text.x = element_text(colour="grey4", size=16), axis.text.y = element_text(colour = "grey4", size=16)) + 
  labs(y = "Survival", x = "Time") + theme_grey(base_size=16)
```

![](Leinenger_2018_files/figure-markdown_github/survival%20figure%20Exp%202-1.png)

Experiment 3
------------

Data for 48 participants (**subject**) reading the same 168 sentences (**item**) from Experiment 1. Instead of directly fixating the different target words, all readers fixated the correct target word but received a parafoveal preview that was either identical, the homophone, or the orthographic control word. Previews were presented using the gaze-contingent invisible boundary paradigm (Rayner, 1975).

**ffd** = first fixation duration, **sfd** = single fixation duration, **gzd** = gaze duration, **gpt** = go-past time, **tvt** = total time, **skp** = fixation probability (inverse of skipping), **rgi** = regression-in probability, **rgo** = regression-out probability.

    ## 'data.frame':    6403 obs. of  13 variables:
    ##  $ subject  : Factor w/ 48 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ item     : Factor w/ 168 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ condition: Factor w/ 3 levels "1","2","3": 1 2 3 1 2 3 1 2 3 1 ...
    ##  $ ffd      : int  187 NA NA NA 200 180 209 NA NA NA ...
    ##  $ sfd      : int  187 NA NA NA 200 180 NA NA NA NA ...
    ##  $ gzd      : int  187 NA NA NA 200 180 362 NA NA NA ...
    ##  $ tvt      : int  187 NA NA NA 200 180 362 NA NA NA ...
    ##  $ gpt      : int  187 NA NA NA 200 180 362 NA NA NA ...
    ##  $ skp      : int  1 0 0 0 1 1 1 0 0 0 ...
    ##  $ rgi      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ rgo      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ launch   : int  8 NA NA NA 5 6 14 NA NA NA ...
    ##  $ Condition: Factor w/ 3 levels "Control","Homophone",..: 3 2 1 3 2 1 3 2 1 3 ...

    ##   subject item condition ffd sfd gzd tvt gpt skp rgi rgo launch Condition
    ## 1       1    1         1 187 187 187 187 187   1   0   0      8    Target
    ## 2       1    2         2  NA  NA  NA  NA  NA   0   0   0     NA Homophone
    ## 3       1    3         3  NA  NA  NA  NA  NA   0   0   0     NA   Control
    ## 4       1    4         1  NA  NA  NA  NA  NA   0   0   0     NA    Target
    ## 5       1    5         2 200 200 200 200 200   1   0   0      5 Homophone
    ## 6       1    6         3 180 180 180 180 180   1   0   0      6   Control

### Means & Standard Errors for Exp. 3

``` r
byCond <- group_by(data, Condition)
stats.m <- summarize_if(byCond,is.numeric,funs(mean), na.rm=TRUE)
stats.se <- summarize_if(byCond,is.numeric,funs(std.error), na.rm=TRUE)
stats.m
```

    ## # A tibble: 3 x 10
    ##   Condition   ffd   sfd   gzd   tvt   gpt   skp   rgi    rgo launch
    ##   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
    ## 1 Control    241.  244.  260.  290.  307. 0.655 0.146 0.0829   6.61
    ## 2 Homophone  235.  237.  253.  286.  296. 0.661 0.157 0.0715   6.63
    ## 3 Target     220.  221.  231.  257.  265. 0.654 0.109 0.0618   6.70

``` r
stats.se
```

    ## # A tibble: 3 x 10
    ##   Condition   ffd   sfd   gzd   tvt   gpt    skp     rgi     rgo launch
    ##   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl>   <dbl>  <dbl>
    ## 1 Control    2.20  2.35  2.73  3.59  4.93 0.0103 0.00768 0.00599 0.0911
    ## 2 Homophone  2.07  2.21  2.60  3.46  4.93 0.0103 0.00788 0.00559 0.0927
    ## 3 Target     1.89  2.00  2.34  3.31  4.42 0.0103 0.00672 0.00519 0.0941

### Figure of early measures for Exp. 3

As with Experiment 1, the graded processing is clearly visible, where readers have an advantage to processing a phonologically related word over a control word matched on visual similarity. This demonstrates that the advantage to processing the phonologically related word was not driven by the somewhat strange task of reading contextually inappropriate words, which may have inflated fixation times, especially in the control condition.

![](Leinenger_2018_files/figure-markdown_github/descriptives%20plot%20exp3-1.png)

### Survival Analyses for Exp. 3

Survival analyses were again conducted to determine just how rapidly phonological codes come online and begin to influence behavior, The DPE reflects the earliest observable influence of phonological coding on behavior.

``` r
tmp<-select(data,subject,ffd,condition) %>% arrange(condition) %>%
  rename(duration=ffd)
tmp$condition <- as.numeric(tmp$condition)
survdata<- as.data.frame(tmp %>% as_tibble() %>% mutate(condition = condition-1)) %>%
  filter(condition != 0, !is.na(duration))
survdata$condition <- as.factor(survdata$condition)
str(survdata)
```

    ## 'data.frame':    2797 obs. of  3 variables:
    ##  $ subject  : Factor w/ 48 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ duration : int  200 294 191 246 315 222 230 176 273 384 ...
    ##  $ condition: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...

We have 48 participants who each have between 33 and 84 data points:

``` r
n.per.sbj <- table(survdata$subject)
length(n.per.sbj)
```

    ## [1] 48

``` r
range(n.per.sbj)
```

    ## [1] 33 84

We can now use these data to generate divergence point estimates (DPE) for each participant:

``` r
ip.dpa <- DPA.ip(survdata$subject, survdata$duration, survdata$condition, quiet = TRUE)
dpe <- as.data.frame(ip.dpa$dp_matrix)
# critical columns in output
# 'dpcount' = the number of iterations (out of 1000) on which a DPE was obtained
#' median_dp_duration' = median of the DPEs obtained on each iteration
str(dpe)
```

    ## 'data.frame':    48 obs. of  6 variables:
    ##  $ subject        : Factor w/ 48 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dpcount        : num  1000 1000 1000 1000 1000 1000 1000 881 1000 1000 ...
    ##  $ median_dp_point: num  1 1 1 278 150 64 51 694 383 1 ...
    ##  $ median_duration: num  132 138 114 208 174 ...
    ##  $ ci.lower       : num  132 138 114 192 166 ...
    ##  $ ci.upper       : num  132 138 114 208 184 ...

``` r
dpe$subject[dpe$dpcount<500]
```

    ## [1] 14 27
    ## 48 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 ... 48

Doing so reveals that the DPE for 2 participants were unreliable (i.e., a DP was found on fewer than half of the iterations). Removing those participants reveals a mean DPE of ~170 across the remaining participants (the value moves around ever so slightly each time the bootstrap re-sampling procedure runs). This tells us that on average, phonological coding was influencing behavior by as early as 170 ms after fixation on the target word began.

``` r
dpe.rel <- filter(dpe, dpcount >= 500)
summarize(dpe.rel, mean.dpe = mean(median_duration, na.rm=TRUE))
```

    ##   mean.dpe
    ## 1 169.8804

Finally, we can represent this visually by examining the survival curves created using the ggsurv function. **Note**--the values displayed on this figure are from the published version of the manuscript and might vary by ~ 1ms from those generated here.

``` r
data$survdat<-as.integer(ifelse(data$Condition=="Target",NA,data$ffd))
tmp2 <- filter(data, !subject %in% c(14, 27))
ffd.surv <- survfit(Surv(survdat) ~ Condition, data=tmp2)
pl2<-ggsurv(s=ffd.surv)

pl2 + geom_vline(xintercept = 170.32, linetype = "dotted") + 
  annotate("rect", xmin=155, xmax=186, ymin=0, ymax=1, alpha = .2) + 
  annotate("text", x = 450, y = 0.75, label = "Divergence Point = 170ms", size = 5) +
  annotate("text", x = 450, y = 0.7, label = "95% CI: 155 - 186ms", size = 5) + 
  theme(axis.text.x = element_text(colour="grey4", size=16), axis.text.y = element_text(colour = "grey4", size=16)) + 
  labs(y = "Survival", x = "Time") + theme_grey(base_size=16)
```

![](Leinenger_2018_files/figure-markdown_github/survival%20figure%20Exp%203-1.png)

Experiment 4
------------

Data for 48 participants (**subject**) reading the 180 sentences (**item**) from Experiment 2, with the word-type manipulation done using the gaze-contingent boundary paradigm like Experiment 3, such that all readers directly fixated the correct target after having had a preview that was either identical, a pseudohomophone, or an orthographic control non-word.

**ffd** = first fixation duration, **sfd** = single fixation duration, **gzd** = gaze duration, **gpt** = go-past time, **tvt** = total time, **skp** = fixation probability (inverse of skipping), **rgi** = regression-in probability, **rgo** = regression-out probability.

    ## 'data.frame':    6704 obs. of  13 variables:
    ##  $ subject  : Factor w/ 48 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ item     : Factor w/ 180 levels "1","2","3","4",..: 1 2 3 4 5 7 8 9 10 11 ...
    ##  $ condition: Factor w/ 3 levels "1","2","3": 1 2 3 1 2 1 2 3 1 2 ...
    ##  $ ffd      : int  NA 173 196 175 NA 215 198 143 195 130 ...
    ##  $ sfd      : int  NA 173 NA 175 NA 215 198 143 195 130 ...
    ##  $ gzd      : int  NA 173 317 175 NA 215 198 143 195 130 ...
    ##  $ tvt      : int  107 173 317 175 NA 215 198 197 195 130 ...
    ##  $ gpt      : int  NA 264 317 175 NA 215 198 143 195 130 ...
    ##  $ skp      : int  0 1 1 1 0 1 1 1 1 1 ...
    ##  $ rgi      : int  1 0 0 0 0 0 0 1 0 0 ...
    ##  $ rgo      : int  0 1 0 0 0 0 0 0 0 0 ...
    ##  $ launch   : int  NA 7 8 2 NA 5 7 5 13 7 ...
    ##  $ Condition: Factor w/ 3 levels "Control","Pseudohomophone",..: 3 2 1 3 2 3 2 1 3 2 ...

    ##   subject item condition ffd sfd gzd tvt gpt skp rgi rgo launch
    ## 1       1    1         1  NA  NA  NA 107  NA   0   1   0     NA
    ## 2       1    2         2 173 173 173 173 264   1   0   1      7
    ## 3       1    3         3 196  NA 317 317 317   1   0   0      8
    ## 4       1    4         1 175 175 175 175 175   1   0   0      2
    ## 5       1    5         2  NA  NA  NA  NA  NA   0   0   0     NA
    ## 6       1    7         1 215 215 215 215 215   1   0   0      5
    ##         Condition
    ## 1          Target
    ## 2 Pseudohomophone
    ## 3         Control
    ## 4          Target
    ## 5 Pseudohomophone
    ## 6          Target

### Means & Standard Errors for Exp. 4

``` r
byCond <- group_by(data, Condition)
stats.m <- summarize_if(byCond,is.numeric,funs(mean), na.rm=TRUE)
stats.se <- summarize_if(byCond,is.numeric,funs(std.error), na.rm=TRUE)
stats.m
```

    ## # A tibble: 3 x 10
    ##   Condition         ffd   sfd   gzd   tvt   gpt   skp    rgi    rgo launch
    ##   <fct>           <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 Control          238.  243.  257.  276.  291. 0.716 0.105  0.0740   6.05
    ## 2 Pseudohomophone  232.  235.  249.  271.  287. 0.727 0.100  0.0740   6.02
    ## 3 Target           215.  214.  229.  250.  256. 0.685 0.0851 0.0597   6.22

``` r
stats.se
```

    ## # A tibble: 3 x 10
    ##   Condition     ffd   sfd   gzd   tvt   gpt     skp     rgi     rgo launch
    ##   <fct>       <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>  <dbl>
    ## 1 Control      1.93  2.06  2.32  3.13  4.03 0.00952 0.00647 0.00553 0.0789
    ## 2 Pseudohomo…  1.80  1.90  2.22  3.19  5.36 0.00947 0.00638 0.00556 0.0815
    ## 3 Target       1.81  1.83  2.28  3.18  3.67 0.00981 0.00589 0.00500 0.0854

### Figure of early measures for Exp. 4

As with with the previous 4 experiments, we again see the graded processing indicative of an advantage to processing phonologically related previews.

![](Leinenger_2018_files/figure-markdown_github/descriptives%20plot%20exp4-1.png)

### Survival Analyses for Exp. 4

Survival analyses were again conducted to determine just how rapidly phonological codes come online and begin to influence behavior, The DPE reflects the earliest observable influence of phonological coding on behavior.

``` r
tmp<-select(data,subject,ffd,condition) %>% arrange(condition) %>%
  rename(duration=ffd)
tmp$condition <- as.numeric(tmp$condition)
survdata<- as.data.frame(tmp %>% as_tibble() %>% mutate(condition = condition-1)) %>%
  filter(condition != 0, !is.na(duration))
survdata$condition <- as.factor(survdata$condition)
str(survdata)
```

    ## 'data.frame':    3218 obs. of  3 variables:
    ##  $ subject  : Factor w/ 48 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ duration : int  173 198 130 221 179 246 145 167 114 134 ...
    ##  $ condition: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...

We have 48 participants who each have between 34 and 98 data points:

``` r
n.per.sbj <- table(survdata$subject)
length(n.per.sbj)
```

    ## [1] 48

``` r
range(n.per.sbj)
```

    ## [1] 34 98

We can now use these data to generate divergence point estimates (DPE) for each participant:

``` r
ip.dpa <- DPA.ip(survdata$subject, survdata$duration, survdata$condition, quiet = TRUE)
dpe <- as.data.frame(ip.dpa$dp_matrix)
# critical columns in output
# 'dpcount' = the number of iterations (out of 1000) on which a DPE was obtained
#' median_dp_duration' = median of the DPEs obtained on each iteration
str(dpe)
```

    ## 'data.frame':    48 obs. of  6 variables:
    ##  $ subject        : Factor w/ 48 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dpcount        : num  1000 1000 995 1000 1000 1000 909 1000 0 1000 ...
    ##  $ median_dp_point: num  241 426 1 127 1 55 178 1 NA 154 ...
    ##  $ median_duration: num  136 232 116 178 132 ...
    ##  $ ci.lower       : num  136 208 116 178 132 ...
    ##  $ ci.upper       : num  136 234 289 178 167 ...

``` r
dpe$subject[dpe$dpcount<500]
```

    ## [1] 9  20 22 29 33 37 39 43
    ## 48 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 ... 48

Doing so reveals that the DPE for 8 participants were unreliable (i.e., a DP was found on fewer than half of the iterations). Removing those participants reveals a mean DPE of ~160 across the remaining participants (the value moves around ever so slightly each time the bootstrap re-sampling procedure runs). This tells us that on average, phonological coding was influencing behavior by as early as 160 ms after fixation on the target word began.

``` r
dpe.rel <- filter(dpe, dpcount >= 500)
summarize(dpe.rel, mean.dpe = mean(median_duration, na.rm=TRUE))
```

    ##   mean.dpe
    ## 1   160.55

Finally, we can represent this visually by examining the survival curves created using the ggsurv function. **Note**--the values displayed on this figure are from the published version of the manuscript and might vary by ~ 1ms from those generated here.

``` r
data$survdat<-as.integer(ifelse(data$Condition=="Target",NA,data$ffd))
tmp2 <- filter(data, !subject %in% c( 9, 20, 22, 29, 33, 37, 39, 43))
ffd.surv <- survfit(Surv(survdat) ~ Condition, data=tmp2)
pl2<-ggsurv(s=ffd.surv)

pl2 + geom_vline(xintercept = 159.81, linetype = "dotted") + 
  annotate("rect", xmin=147, xmax=172, ymin=0, ymax=1, alpha = .2) + 
  annotate("text", x = 450, y = 0.75, label = "Divergence Point = 160ms", size = 5) +
  annotate("text", x = 450, y = 0.7, label = "95% CI: 147 - 172ms", size = 5) + 
  theme(axis.text.x = element_text(colour="grey4", size=16), axis.text.y = element_text(colour = "grey4", size=16)) + 
  labs(y = "Survival", x = "Time") + theme_grey(base_size=16)
```

![](Leinenger_2018_files/figure-markdown_github/survival%20figure%20Exp%204-1.png)
