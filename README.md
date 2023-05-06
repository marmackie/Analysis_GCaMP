# Final Independent Project - Marisa Mackie

This repository will contain all data, scripts, and outputs related to my Final Independent Project.

### Background:
This project is related to my Master's Thesis project:

**Mapping chemical stimuli to their cognate neurons in _Pristionchus pacificus_ via genetically encoded calcium indicators**


I work with nematode (microscopic roundworm) species _Pristionchus pacificus_ and am interested in studying their amphid neurons, which are responsible for the sensing of chemicals in their environment--that is, odorants and tastants. There are 12 pairs of these amphid neurons, however, it is unknown which neurons specifically are responsible for sensing particular volatile odors or water-soluble tastants. My goal is to map chemical stimuli to their cognate neurons. 

To do this, I have used genetically-encoded calcium indicators (GECIs) (i.e., GCaMP) to measure neuron response to chemical stimuli in real time. Neuron response is measured as percent change in fluorescence intensity (df/f) over time. Raw data are collected in the form of text files (.log) with 9 columns of comma-delimited data.

### Purpose:

The purpose of my script is to grab my raw data files from each folder, calculate df/f, average these values, and visualize them on plots.


### Structure:

In the **data** folder, you will find all folders containing all text (.log) files that will be analyzed, as well as a data dictionary.

In the **scripts** folder, you will find all code used to analyze the data.

In the **outputs** folder, you will find all plots and other outputs that come from completed data analysis.