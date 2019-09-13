set yrange [0:1400]
set grid
set key box height 0.75 width -2.0 at graph 0.85, 0.3
set terminal cairolatex
set ylabel 'Anzahl Unfälle'
set output 'img/anzahl-unfaelle-pro-jahr1.tex'
plot 'anzahl-unfaelle-pro-jahr.data' using 1:2 with lines title 'Anzahl Unfälle mit Personenschaden' linetype 1 dashtype 1 linewidth 5
