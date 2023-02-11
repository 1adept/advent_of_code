#! /bin/bash

cur_year=$(date '+%Y')
cur_day=$(date '+%d')
number='^[0-9]+$'

read -p "Please enter your token: You can get it from the input-site under 'Cookies': " token
if [[ -z $token ]]; then
    echo "error: No token given" >&2; exit 1
fi

read -p "Enter year. Default current Year ($(date +%Y)): " year
if [[ -z $year ]]; then
    if [[ $cur_year -ge $year ]]; then
        year=$((cur_year - 1))
    else
        year=$cur_year
    fi
elif [[ $year -gt $cur_year ]]; then
    echo "error: Year $year is in the future" >&2; exit 1
elif ! [[ $year =~ $number ]]; then
    echo "error: $year is not a number" >&2; exit 1
elif [[ ${#year} == 2 ]]; then
    year="20$year"
fi

read -p "Enter day: Empty defaults to all days (if past year) or current day ($cur_day)" day
if [[ -z $day ]]; then
    if [[ $year == $cur_year ]]; then
        day=($cur_day)
    else 
        day=({01..25})
    fi
elif ! [[ $day =~ $number ]]; then
    echo "error: $day is not a number" >&2; exit 1
fi

if [[ ${#day} == 1 ]]; then
    day=$(printf %02d $day)
    echo $day
fi

# Creating folder
dir="$year/input"
if ! [[ -e $dir ]]; then
    mkdir -p $dir
fi

# Download input for each day
for d in ${day[@]}; do
    day_file="$dir/day$d.in"
    if ! [[ -e $day_file ]]; then
        touch $day_file
        url="https://adventofcode.com/$year/day/${d##0}/input"
        curl --cookie "session=$token" $url -o $day_file
    fi
done