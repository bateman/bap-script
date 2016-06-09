#!/bin/bash
printf " === Starting the tuning of classifiers' params (this may take a while...)\n"
start_time=$(date +"%Y-%m-%d_%H.%M")
for i in `seq 1 10`;
do
        echo " :: Run $i"
        time Rscript tuning.R $i $start_time
done   

echo " Done"
