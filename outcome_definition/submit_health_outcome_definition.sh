#PBS -l walltime=2:00:00
#PBS -l select=1:ncpus=8:mem=100gb
#PBS -N definition
#PBS -J 3-4

cd /rds/general/user/fg520/home/CHD/outcome_definition/Scripts

module load anaconda3/personal
source activate phd_r

def_path=/rds/general/user/fg520/home/CHD/outcome_definition/Definitions/out${PBS_ARRAY_INDEX}*/

out_path=/rds/general/user/fg520/home/CHD/outcome_definition/Outputs/out${PBS_ARRAY_INDEX}/

app_data_path=/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_677583/ukb677583.csv

hes_main_path=/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_677583/hesin.txt

hes_diag_path=/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_677583/hesin_diag.txt

hes_oper_path=/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_677583/hesin_oper.txt

death_main_path=/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_677583/death.txt

death_cause_path=/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_677583/death_cause.txt

Rscript extract_hes.R $def_path $app_data_path $hes_main_path $hes_diag_path $hes_oper_path $out_path

Rscript extract_death.R $def_path $app_data_path $death_main_path $death_cause_path $out_path

Rscript extract_baseline.R $def_path $app_data_path $out_path
