#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=100gb
#PBS -N recoding
#PBS -q med-bio


cd /rds/general/user/fg520/home/CHD/extraction_and_recoding/scripts

module load anaconda3/personal

Rscript 3-recode_variables.R
