#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=50gb
#PBS -N extraction
#PBS -q med-bio

module load anaconda3/personal
source activate phd_r

cd /rds/general/user/fg520/home/CVD/extraction_and_recoding/scripts

ukb_path=/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_673609/ukb673609.csv

Rscript 2-extract_selected.R $ukb_path 

conda deactivate
