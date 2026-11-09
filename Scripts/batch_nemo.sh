case="EXP_8p2ka_ctrl"
cfg="NAARC"

iy1=2009 # initial year
iy2=2010 # end year
ms=10 # month start
im2=12 # month end
nn_leap=30 # month length
init=0 # first initialisation

rDt=300

tpd=$((86400 / $rDt))

SRUN_CMD=$1

if [ $init -eq 1 ]; then
   nit0=1
   #tpd=$(($tpd*10))
else
   # set nit0
   if [ $ms -eq 1 ]; then 
   	mm=12
   	yyyy=$((iy1 - 1))
   else
          	mm=$(printf "%02d" $((ms - 1)))
   	yyyy=$iy1
   fi
   itm1=$(grep "nn_itend" $yyyy/$mm/namelist_cfg | grep -o -E "[0-9]+" | sort -n | tail -1 )
   nit0=$((itm1 + 1))
fi

## year and month are the formatted version of running month and year to be used for script launching
## y and m are the numeric one to be used in counting
if [ $im2 -ge 13 ]; then im2=12; fi   # if out of step at end of year just run to end of year

# loop for years
yy=$iy1
while [  $yy -le $iy2 ]; do

   # loop for years
   year=${yy}
   
   #loop for months
   if [ $yy -eq $iy1 ]; then mm=$ms; else mm=1; fi
   while [ $mm -le $im2 ]; do
      
      if [ $mm -lt 10 ] ; then
         month=0$mm
      else
         month=$mm
      fi
      
      date
      
      echo "Running year $year, month $month ..."
      #get run length in iterations (nit)
      if [ $nn_leap -lt 2 ] ; then
        case $month in
                04|06|09|11) nit=$((30*tpd)) ;;
                #february considering leap years:
                #02) if [ `expr $year % 4` -ne 0 -o `expr $year % 100` -eq 0 -a `expr $year % 400` -ne 0 ]; then nit=$((28*tpd)); else nit=$((29*tpd)); fi ;;
                #february not considering leap years:
                02) nit=$((28*tpd)) ;;
                *) nit=$((31*tpd)) ;;
        esac
      else
        nit=$((nn_leap*tpd))
      fi
      nitend=`expr $nit0 + $nit`
      nitend=`expr $nitend - 1`
      
      if [ $nit0 -eq 1 ] ; then
         rstart_ice=0  # start from sst
         rstart=.false.  # start from rest
         rstctl=0
         inits=.true.    # start from rest
         nitrst=00000000
      else
         rstart_ice=2  # start from restart
         rstart=.true.   # start from restart
         rstctl=2
         inits=.false.   # start from restart
         nitrst=$year$month'01'
      fi
      
      cat namelist_cfg.template \
          | sed "s,__IT000__,$nit0,g" \
          | sed "s,__ITEND__,$nitend,g" \
          | sed "s,__RSTART__,$rstart,g" \
          | sed "s,__INITS__,$inits,g" \
          | sed "s,__RSTNIT__,$nitrst,g" \
          | sed "s,__RSTCTL__,$rstctl,g" \
          | sed "s,__RDT__,$rDt,g" \
          > namelist_cfg

      cat namelist_ice_cfg.template \
          | sed "s,__RSTNIT__,$nitrst,g" \
          | sed "s,__RSTART_ICE__,$rstart_ice,g" \
          > namelist_ice_cfg

      # move previous restarts
      mv RESTART_OUT/VERIFY_${nitrst}_restart* RESTART_IN/
      
      # this is the RUN bit ------>>>>>
      
      echo $SRUN_CMD
      eval $SRUN_CMD
      
      if [ $? -ne 0 ] 
      then
         exit
      fi

      ## end of run bit <-----
      date

      OUTDIR=$(pwd)/$year/$month

      # if succesful and not January remove restart input, else move to be transferred
      echo "test"
      if [ $mm -ne 1 ]
      then
         #year_m1=$(date --date "$year-$month-01 -1 month" "+%Y")
         #month_m1=$(date --date "$year-$month-01 -1 month" "+%m")
         #test_date=$(date --date "$year-$month-01 +1 month" "+%Y%m")
         #echo "$(pwd)/$year_m1/$month_m1/RESTARTS"
         #rm $(pwd)/$year_m1/$month_m1/RESTARTS/VERIFY_${year_m1}${month_m1}01_restart.nc
         #rm $(pwd)/$year_m1/$month_m1/RESTARTS/VERIFY_${year_m1}${month_m1}01_restart_ice.nc
         rm RESTART_IN/VERIFY_${nitrst}_restart.nc
         rm RESTART_IN/VERIFY_${nitrst}_restart_ice.nc
      else
         mkdir -p ${OUTDIR}/RESTARTS
         mv RESTART_IN/VERIFY_${nitrst}_restart* $OUTDIR/RESTARTS/.
      fi
      echo "test"

      ##
      ### move output files
      mkdir -p $OUTDIR
      mv ocean.output $OUTDIR/ocean.output
      mv namelist_cfg $OUTDIR/namelist_cfg
      mv namelist_ice_cfg $OUTDIR/namelist_ice_cfg
      mv VERIFY_6h_*  $OUTDIR/.
      mv VERIFY_5d_*  $OUTDIR/.
      mv VERIFY_1m_*  $OUTDIR/.
      mv mon*  $OUTDIR/.
      mv shelf*  $OUTDIR/.
      mv run.stat $OUTDIR/.

      # move data to jasmin using globus
      # --wait enforces the jobs exit code rather than the submission exit code
      sbatch globus_transfer.slurm $cfg $case $year $month


      current_stp=`sed -n 1,1p time.step`
      if [ ! $current_stp -eq $nitend ]
      then
         exit
      fi
      
      nit0=`expr $nitend + 1`

      
      mm=$((mm+1))
   done # 1..12 months

   #echo 'exit code with ?'
   #echo $?
   #if [ $? -eq 0 ] 
   #then
   #   rm -r ${yy}/{01..11}
   #else
   #   echo 'transfer failed'
   #fi

   #exit_code=$(sacct -X -j "$jobid" --format=ExitCode --noheader | awk -F: '{print $1}')
   #cho "$exit_code"
      
   yy=$((yy+1))
done # year and year+1

#if [ $y1 -lt 2006 ]; then
#echo qsub -v I=$I,J=$J,IJ=$IJ,m=$m1,y=$y1,nit0=$nit0  -o $RUNDIR/AMM7-$IJ-$y1-$month1 -N AMM7$y1$month1 monthlyrun.pbs
#qsub -v I=$I,J=$J,IJ=$IJ,m=$m1,y=$y1,nit0=$nit0 -o $RUNDIR/NE-AMM7-$IJ-$y1-$month1 -N NE$y1$month1  monthlyrun.pbs
#fi

exit


