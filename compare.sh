#!/bin/bash

cd /home/ec2-user/drunk/
Rscript /home/ec2-user/drunk/2110/compare.R 2> erro_$(date -d "today" +"%Y%m%d%H%M").txt
