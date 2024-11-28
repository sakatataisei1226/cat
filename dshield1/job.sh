#!/bin/bash
#============ Slurm Options ===========
#SBATCH -p gr10451a
#SBATCH -t 24:00:00
#SBATCH --rsc p=112:t=1:c=1:m=1070M
#SBATCH -o %x.%j.out

#============ Shell Script ============
module load hdf5/1.12.2_intel-2022.3-impi
module load fftw/3.3.10_intel-2022.3-impi

date
srun ./mpiemses3D plasma.inp
echo ...done
/LARGE2/gr10451/share/y-miyake/anaconda3/bin/python3 generate_xdmf3.py nd1p00_0000.h5 nd2p00_0000.h5 phisp00_0000.h5
/LARGE2/gr10451/share/y-miyake/anaconda3/bin/python3 generate_xdmf3.py ex00_0000.h5 ey00_0000.h5 ez00_0000.h5
/LARGE2/gr10451/share/y-miyake/anaconda3/bin/python3 generate_xdmf3.py j1x00_0000.h5 j1y00_0000.h5 j1z00_0000.h5 j2x00_0000.h5 j2y00_0000.h5 j2z00_0000.h5
date
