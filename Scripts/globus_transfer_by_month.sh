cfg="NAARC"
case="EXP_8p2ka_ctrl"
year=1993
for month in {03,}; do  
    source /work/n02/n02/ryapat30/nemo/Scripts/cli-sync.sh $cfg $case $year $month;
done
