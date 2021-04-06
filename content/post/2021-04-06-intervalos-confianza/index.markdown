---
title: Intervalos de Confianza
author: Paul Efren
date: "06 April, 2021"
categories:
  - R
tags:
  - rmarkdown
slug: new-year-new-blogdown
authors:
  - paul
summary:
image:
  caption: ''
  focal_point: ''
---




This is a modification of a blog written by **Sean Kross**, using **tidyverse**.

Esta es una modificación de un blog escrito por **Sean Kross**[^note1], utilizando **tidyverse**[^note2].

## Data:

- Imagina que estás buscando estimar  el promedio de una alguna característica de una población.


<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-1.png" width="672" />

 - Conformada por 100 000 individuos. 


```r
set.seed(2016-05-04)
poblacion <- tibble(individuo = seq(1,100000),
                         caracteristica = runif(100000))
```


```
New names:
* individuo -> individuo...1
* caracteristica -> caracteristica...2
* individuo -> individuo...3
* caracteristica -> caracteristica...4
```



| Individuo| Caracteristica| Individuo| Caracteristica|
|---------:|--------------:|---------:|--------------:|
|         1|      0.6770751|     99995|      0.2685845|
|         2|      0.2718245|     99996|      0.6156618|
|         3|      0.1031951|     99997|      0.5487510|
|         4|      0.6260607|     99998|      0.9394554|
|         5|      0.4148852|     99999|      0.2003218|
|         6|      0.1208850|    100000|      0.3245546|

- Lo que estamos buscando es determinar el promedio  real de esta población **0.4994207**.

- Pero normalmente no podemos medir cada uno de los individuos de esta población. Entonces recurrimos a tomar muestras:


```r
muestra <- poblacion %>% sample_n(100) 
```


```
New names:
* individuo -> individuo...1
* caracteristica -> caracteristica...2
* individuo -> individuo...3
* caracteristica -> caracteristica...4
```



| Individuo| Caracteristica| Individuo| Caracteristica|
|---------:|--------------:|---------:|--------------:|
|     25935|      0.6259550|     97415|      0.6233856|
|     81902|      0.8895348|     67751|      0.4995026|
|     92418|      0.6939808|      2396|      0.1324094|
|     97217|      0.6266913|     78110|      0.2946156|
|     15679|      0.3646615|     33854|      0.5986135|
|     60239|      0.0858186|     37290|      0.5811122|

- Ahora con esta muestra podemos construir el **intervalo de confianza** al  95%:


```r
# Funcion 
ci <- function(x) {
  ci <- 1.96 * (sd(x)/sqrt(length(x)))
  return(ci) 
}

muestra %>% 
  summarise(
    Lim_inf = mean(caracteristica) - ci(caracteristica),
    Promedio = mean(caracteristica),
    Lim_sup = mean(caracteristica) + ci(caracteristica)) %>% 
  kable()
```


|   Lim_inf|  Promedio|   Lim_sup|
|---------:|---------:|---------:|
| 0.4475891| 0.5028541| 0.5581192|

## Intervalo de confianza?

Si tomamos varias muestras de una población, el promedio real de esta debería ser contenido por el intervalo de confianza de la muestra.

Tomemos 10000 muestras de la población.



```r
poblacion %>% 
  rep_sample_n(size = 100, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(ci = ci(caracteristica),
            mean = mean(caracteristica)) %>% 
  kable()
```


| Replica|  Promedio|  Lim_inf.|  Lim_sup.|
|-------:|---------:|---------:|---------:|
|       1| 0.4982383| 0.4376601| 0.5588165|
|       2| 0.4391961| 0.3857278| 0.4926644|
|       3| 0.5200138| 0.4671503| 0.5728772|
|       4| 0.5029914| 0.4464481| 0.5595348|
|       5| 0.4905542| 0.4332180| 0.5478904|
|       6| 0.5484101| 0.4901481| 0.6066722|
|    9995| 0.5042877| 0.4491597| 0.5594156|
|    9996| 0.5070906| 0.4504482| 0.5637330|
|    9997| 0.4899686| 0.4321288| 0.5478085|
|    9998| 0.4953093| 0.4373808| 0.5532377|
|    9999| 0.4822687| 0.4239676| 0.5405697|
|   10000| 0.4602124| 0.4009354| 0.5194894|

- Cual es el porcentaje de muestras que capturan el promedio real de la población?


```r
replicas %>%
  mutate(prom_real =
        case_when(lim_inf < 0.4994207 & 0.4994207<lim_sup ~ 1, TRUE ~ 0)) %>%
  summarise(mean(prom_real)) %>% 
  kable()
```



- El **0.95** porciento de las muestras captura el promedio real de la población. 

## Visualización

- Visualización grafica de los intervalos de confianza.



```r
replicas %>% 
  sample_n(100) %>% 
  ggplot(aes(replicate, prom))+
  geom_pointrange(aes(ymin =lim_inf, ymax =lim_sup), 
                  size = 0, color = "#1B7837")+
  theme_bw()+
  geom_hline(yintercept = mean(poblacion$caracteristica),
             color = "red", size = 1.2)+
  labs(x = "Replicas",
       y = "")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

## Referencias

[^note1]: Basado en **A Short Intro to Confidence Intervals** por [Sean Kross](http://seankross.com/).

[^note2]: [Tidyverse](https://www.tidyverse.org/).




