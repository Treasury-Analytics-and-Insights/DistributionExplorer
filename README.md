# Introduction
This package contains prototype software for running the Income Distribution 
Explorer application. This software was developed by an intern who joined the
Analytics and Insights team during the summer of 2021/22 as part of the Analytics
and Research in Government internship programme.

The Income Distribution Explorer is a tool which can be used to understand the
income distribution for households and families in New Zealand.

The tool allows users to explore and compare the income distribution for 
subgroups of interest, for example households with individuals aged over 65, 
households with (and without) children, and households in receipt of different 
welfare payments.

# Overview

To run, open the distExplApp.R program in RStudio and click RunApp. 

This should open a browser window (like Microsoft Edge or Chrome) with the Income 
Distribution Explorer app running.

From there you can click Browse (top left of window) and select the attached Excel 
file containing the income estimates (DistExpl_HES20_TY20_HYEFU21.xlsx)

# Notes

Income estimates were produced using Treasury’s micro-simulation model of the tax and 
welfare system - TAWA. Income estimates are for the 2019/20 tax year using HES 2019/20 
augmented using IDI data, inflated and population adjusted with HYEFU21 inflation 
estimates.

# Disclaimer

This code can be modified and customised by users to meet the needs of specific
projects, and in all cases the Analytics and Insights Team,
Te Tai Ōhanga, New Zealand Treasury must be acknowledged as a source.
While all care and diligence has been used in developing this code,
The Treasury gives no warranty it is error free and will not be liable for any
loss or damage suffered as a result of its use, either directly or indirectly.
