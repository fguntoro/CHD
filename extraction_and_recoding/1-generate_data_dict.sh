#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=20gb

module load anaconda3/personal
source activate phd_r

cd /rds/general/user/fg520/home/CHD/extraction_and_recoding/scripts

ukb_path=/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_677583/ukb677583.csv
pwd

Rscript 1-make_data_dict.R $ukb_path

conda deactivate
echo "stop"