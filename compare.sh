#!/bin/bash

cd /home/ec2-user/dunk/
Rscript /home/ec2-user/dunk/2110/compare.R 2> erro_$(date -d "today" +"%Y%m%d%H%M").txt
