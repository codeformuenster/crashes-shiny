set yrange [0:1350]
set terminal cairolatex
set ylabel 'Anzahl Unfälle mit Personenschaden'
set output 'img/anzahl-unfaelle-pro-jahr.tex'
plot 'anzahl-unfaelle-pro-jahr.data' using 1:2 with lines
