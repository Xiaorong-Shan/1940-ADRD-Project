#!/bin/bash
#SBATCH   --partition=bigmem            # submit   to the normal(default) partition
##SBATCH   --qos=normal
##SBATCH   --constraint=amd
##SBATCH   --nodelist=amd090
#SBATCH   --job-name=com_pt             # name the job
#SBATCH   --output=com_pt-%x-%N-%A-%a.out        # write stdout/stderr   to named file
#SBATCH   --error=com_pt-%x-%N-%A-%a.err      
#SBATCH   --time=3-00:00:00             # Run for max of 02 hrs, 00 mins, 00 secs
#SBATCH   --nodes=1                    # Request N nodes
#SBATCH   --ntasks-per-node=24            # Request n   cores per node
#SBATCH   --mem-per-cpu=15G             # Request nGB RAM per core
#SBATCH   --mail-user=xshan2@gmu.edu    #Email account
#SBATCH   --mail-type=FAIL,BEGIN,END           #When to email




#load modules with  
module load gnu10
module load openmpi
module load netcdf-c
module load r/4.1.2-dx  #will load default r version

Rscript   NHGIScensus-final.R
