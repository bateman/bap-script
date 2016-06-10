#!/bin/bash

input_file=$1
models_file=$2

if [[ -n "$models_file" && -n "$input_file" ]]; then
    printf " === Starting the tuning of classifiers' params (this may take a while...)\n"
    start_time=$(date +"%Y-%m-%d_%H.%M")
    for i in `seq 1 10`;
    do
        echo " :: Run $i"
        time Rscript tuning.R $i $start_time $models_file $input_file
    done   

    echo " Done"
else
    echo "Argument error: models and/or input file not given."
fi


