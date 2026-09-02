#!/bin/bash
#SBATCH --job-name=abs-fixed-clock
#SBATCH --account=wharton-phd-students
#SBATCH --qos=wharton-phd-student-genoa
#SBATCH --partition=genoa-std-mem
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --mem-per-cpu=5632M
#SBATCH --time=7-00:00:00
#SBATCH --output=slurm-%x-%j.out
#SBATCH --error=slurm-%x-%j.err

set -euo pipefail

project_dir="${ABS_PROJECT_DIR:-${SLURM_SUBMIT_DIR:?Set ABS_PROJECT_DIR or submit from the project root}}"
environment_dir="${ABS_ENVIRONMENT_DIR:-$project_dir/.hpc-env/abs-fixed-clock-r44}"
environment_file="$project_dir/hpc/fixed-clock/betty_fixed_clock_environment.yml"

module load anaconda3/2023.09-0
source "$(conda info --base)/etc/profile.d/conda.sh"

if [[ ! -x "$environment_dir/bin/Rscript" ]]; then
  conda activate MAMBA
  micromamba create --yes --prefix "$environment_dir" --file "$environment_file"
  conda deactivate
fi
conda activate "$environment_dir"

export ABS_FIXED_CLOCK_PROFILE=full
export ABS_FIXED_CLOCK_REFRESH_TARGETS=false
export ABS_FIXED_CLOCK_CONTEXT_RE=true
export ABS_FIXED_CLOCK_CANDIDATE_WORKERS=6
export ABS_FIXED_CLOCK_WORKERS=4
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export R_PROFILE_USER=/dev/null
export R_ENVIRON_USER=/dev/null

run_tmp="${SLURM_TMPDIR:-$project_dir/tmp/slurm-$SLURM_JOB_ID}"
mkdir -p "$run_tmp" "$project_dir/logs"
export TMPDIR="$run_tmp"

cd "$project_dir"
Rscript --vanilla -e 'cat(R.version.string, "\n"); print(sessionInfo())'
Rscript --vanilla analysis/policy/run_fixed_clock_confirmation_1d.R
