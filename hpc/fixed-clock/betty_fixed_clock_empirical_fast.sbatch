#!/bin/bash
#SBATCH --job-name=abs-fc-empirical
#SBATCH --account=wharton-phd-students
#SBATCH --qos=wharton-phd-student-genoa
#SBATCH --partition=genoa-std-mem
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=5632M
#SBATCH --time=2-00:00:00
#SBATCH --output=slurm-%x-%j.out
#SBATCH --error=slurm-%x-%j.err

set -euo pipefail

# Use a new immutable project namespace so the stopped GMM run and its
# checkpoints remain available for audit. Reuse only the software environment.
project_dir="${ABS_PROJECT_DIR:-${SLURM_SUBMIT_DIR:?Set ABS_PROJECT_DIR or submit from the project root}}"
environment_dir="${ABS_ENVIRONMENT_DIR:-$project_dir/.hpc-env/abs-fixed-clock-r44}"
environment_file="$project_dir/hpc/fixed-clock/betty_fixed_clock_environment.yml"
warm_start_policy="$project_dir/data/processed/perception/fixed_clock_confirmation/warm_starts/gmm_development_frozen_policy.rds"

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

# Empirical development prior: eligible opportunities, role x raw count,
# 0.01-inch bins, with count cells shrunk toward their role-wide distribution.
export ABS_FIXED_CLOCK_PRIOR_FAMILY=empirical_binned
export ABS_FIXED_CLOCK_EMPIRICAL_BIN_WIDTH=0.01
export ABS_FIXED_CLOCK_EMPIRICAL_ALPHA_GRID=0,25,50,100,200

# Preserve five-fold OOF validation, but validate only the already selected
# development-only df/ridge setting instead of repeating a four-cell search.
export ABS_FIXED_CLOCK_POLICY_CV=true
export ABS_FIXED_CLOCK_POLICY_CV_GRID=fixed
export ABS_FIXED_CLOCK_FIXED_STAGE_DF=4
export ABS_FIXED_CLOCK_FIXED_RIDGE=0.001
export ABS_FIXED_CLOCK_WARM_START_POLICY="$warm_start_policy"
export ABS_FIXED_CLOCK_RESUME_STAGES=true

# 300 coordinated primary draws. Every third draw also performs the secondary
# local learning-procedure refit, yielding exactly 100 procedure draws.
export ABS_FIXED_CLOCK_BOOTSTRAP_REPS=300
export ABS_FIXED_CLOCK_PROCEDURE_BOOTSTRAP_STRIDE=3
export ABS_FIXED_CLOCK_CANDIDATE_WORKERS=16
export ABS_FIXED_CLOCK_WORKERS=12

# Each fork is deliberately single-threaded; parallelism exists only at the
# scenario/replicate level. Limiting malloc arenas reduces copy-on-write memory.
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export R_DATATABLE_NUM_THREADS=1
export MALLOC_ARENA_MAX=2
export R_PROFILE_USER=/dev/null
export R_ENVIRON_USER=/dev/null

run_tmp="${SLURM_TMPDIR:-$project_dir/tmp/slurm-$SLURM_JOB_ID}"
mkdir -p "$run_tmp" "$project_dir/logs"
export TMPDIR="$run_tmp"

cd "$project_dir"
if [[ ! -f "$warm_start_policy" ]]; then
  echo "Missing development-only warm-start policy: $warm_start_policy" >&2
  exit 2
fi

echo "Started $(date --iso-8601=seconds) on $(hostname)"
echo "SLURM job=$SLURM_JOB_ID cpus=$SLURM_CPUS_PER_TASK memory=88GiB"
Rscript --vanilla -e 'cat(R.version.string, "\n"); print(sessionInfo())'
Rscript --vanilla analysis/policy/run_fixed_clock_confirmation_1d.R
echo "Completed $(date --iso-8601=seconds)"
