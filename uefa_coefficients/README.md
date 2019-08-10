UEFA coefficients animated
==========================

Data source
-----------

Country rankings and coefficients were scraped from
<a href="https://kassiesa.home.xs4all.nl/bert/uefa/data/index.html" class="uri">https://kassiesa.home.xs4all.nl/bert/uefa/data/index.html</a>
using the `rvest` package. The website provides datasets of UEFA country
coefficients from 1960 to today. Note there were multiple changes to the
calculation of the coefficients.

Animations
----------

Animated graphs were created using `gganimate`.

Limited to countries which ever made it to the annual top 5 in order to
reduce the number of items displayed on the chart. Before 1990, West
German Bundesliga is labelled Germany.

![](output/uefa_coefficients_ranks.gif)

![](output/uefa_coefficients_top10.gif)
