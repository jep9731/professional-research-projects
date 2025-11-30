# Hippocampal Segmentation Modality Comparison 🧠

## Objective

This project evaluates hippocampal segmentation performance using different MRI input modalities:

- T1-only  
- T2-only  
- T2 high-resolution (T2H) only  
- T1 + T2  
- T1 + T2H 

Segmentation is performed using **FreeSurfer** software. The goal is to compare accuracy, reliability, and volumetric outcomes across input modalities.

## Modalities
- T1-weighted MRI
- T2-weighted MRI
- T2 high-resolution MRI

## Workflow
- Run segmentation pipelines
    - Use T1 only, T2 only, T2H only, T1 and T2, and T1 and T2H
- Compute volume + reliability comparisons
- Visual overlays for qualitative assessment

## Outputs
- Figures in `results/overlays/`
- Volume CSVs in `results/volumes/`
- Metrics in `results/metrics/`
