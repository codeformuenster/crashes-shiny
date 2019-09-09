#!/bin/sh
rm gnuplot.command

if [ -z "$1" ] 
then
	echo "Please specify .tex output file name.";
	exit 1
fi

texFile=$(echo "${1%.*}")

mkdir -p img/

echo "set yrange [0:1350]" >> gnuplot.command
#echo set style boxplot nooutliers >> gnuplot.command
#echo set offsets 0, 0.15 >> gnuplot.command
# echo set xrange [0.4:4.6] >> gnuplot.command
echo set terminal cairolatex >> gnuplot.command
#echo "set title '$dataFile'" >> gnuplot.command
# echo set style fill transparent solid 0.4 >> gnuplot.command
#echo set boxwidth 0.15 >> gnuplot.command
# echo set xtic rotate by -30 >> gnuplot.command 
echo "set ylabel 'Anzahl Unfälle mit Personenschaden'" >> gnuplot.command
echo "set output 'img/$texFile.tex'" >> gnuplot.command
# plot only box-and-whiskers:
# plot only medians and means:
echo "plot 'anzahl-unfaelle-pro-jahr.data' using 1:2 with lines" >> gnuplot.command
#echo pause -1 >> gnuplot.command

gnuplot gnuplot.command 

