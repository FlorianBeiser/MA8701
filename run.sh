#!/bin/bash
# echo "Hello world"
# # add two numeric values
# # 'The folloing is not going to be printed
# # hahaha, 5
# # '
# : '
# The following script calculates
# the square value of the number, 5.
# '
# : '
# test
# '
# ((sum = 20 + 49))

# echo $sum

# valid=true
# count=1
# while [ $valid ]
# do
# echo $count
# if [ $count -eq 5 ];
# then
# break
# fi
# ((count++))
# done
# echo "end of script"

# for ((counter=10; counter>0; counter--))
# do
# echo -n "$counter"
# done
# printf "/n"

# echo "Enter your name"
# read name
# echo "Welcome $name to our house"


echo "Bot is running"
# s1="git init"
s2="git pull"
s21="git add ."
s22="git commit -m \"updates\""
s3="git push --all"
for ((counter=12;counter>0;counter--))
do 
echo $counter
(eval $s2)
(eval $s21)
(eval $s22)
(eval $s3)
sleep 300
done
echo "ferdig"







