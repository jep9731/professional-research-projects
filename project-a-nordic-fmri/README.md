# Project A — Comparing fMRI Functional Data: NORDIC vs. Non-NORDIC

## Objective
Evaluate NORDIC denoising effects on temporal SNR, stability, and activation outcomes.

## Data
Not included in repo — MUST provide your own MRI data.
See `data/raw/README.md` for guidance.

## Pipeline
- Denoising (NORDIC vs. baseline)
- Motion QC
- fMRI preprocessing
- Activation + tSNR comparisons

## Run analysis
```python src/analysis/run_pipeline.py```

## Outputs

Results stored in:

* `results/figures/`
* `results/stats/`
