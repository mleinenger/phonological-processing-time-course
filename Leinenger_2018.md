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

Experiment 2
------------

Data for 48 participants (**subject**) reading 188 sentences (**item**) that either contained the correct target word, a pseudohomophone (i.e., phonologically related non-word), or an orthographic control non-word.

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
