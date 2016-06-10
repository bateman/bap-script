#!/bin/bash

models_file=$1

if [[ -n "$models_file" ]]; then
    printf " === Starting the tuning of classifiers' params (this may take a while...)\n"
    start_time=$(date +"%Y-%m-%d_%H.%M")
    for i in `seq 1 10`;
    do
        echo " :: Run $i"
        time Rscript tuning.R $i $start_time
    done   

    echo " Done"
else
    echo "Argument error: no models file given."
fi


