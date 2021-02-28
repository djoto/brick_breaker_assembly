#!/usr/bin/env bash

declare -i num
num=$(ls | wc -l)

function check {
        until read -p "Enter your guess: " guess;
        [[ "$guess" =~ ^[0-9]+$ ]]; do
                echo -e "\nPlease enter unnegative integer!\n"
        done
}

while check; do
        if [[ $guess -lt $num ]]; then
                echo -e "\nYour guess is to low, try again.\n"
        elif [[ $guess -gt $num ]]; then
                echo -e "\nYour guess is too high, try again.\n"
        else
                echo -e "\nCongratulation, your guess is correct!"
                exit 
        fi
done
