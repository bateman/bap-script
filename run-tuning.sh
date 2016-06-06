#!/bin/bash
printf " === Starting the tuning of classifiers' params (this may take a while...)\n"
for i in `seq 1 10`;
do
        echo " :: Run $i"
        Rscript tuning.R $i
done   

echo " Done"
