#!/bin/sh
rm gnuplot.command

texFile=anzahl-unfaelle-pro-jahr

mkdir -p img/

echo "set yrange [0:1400]" >> gnuplot.command
# echo set xrange [0.4:4.6] >> gnuplot.command
echo "set grid" >> gnuplot.command
echo "set key box height 0.75 width -3 at graph 0.85, 0.3" >> gnuplot.command
echo "set terminal cairolatex" >> gnuplot.command
#echo "set title '$dataFile'" >> gnuplot.command
echo "set ylabel 'Anzahl Unfälle'" >> gnuplot.command
echo "set output 'img/$texFile.tex'" >> gnuplot.command
echo "plot 'anzahl-unfaelle-pro-jahr.data' using 1:2 with lines title 'Anzahl Unfälle mit Personenschaden' linetype 1 dashtype 1 linewidth 5, 'anzahl-unfaelle-pro-jahr.data' using 1:3 with lines title 'Ziel Ordnungspartnerschaft (-10\% pro Jahr)' linetype 2 dashtype 4 linewidth 5" >> gnuplot.command
#echo pause -1 >> gnuplot.command

gnuplot gnuplot.command 

