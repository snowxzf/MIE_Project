# Line Tracer Data Visualization

## Overview

This script reads the collected CSV trial data and:

1. Overlays the target curve (reference line) with the participant’s cursor path for each trial.
2. Produces CSVs with aligned columns for **target data** (graph) and **cursor data** (participant).

## Output Structure

### Graphs (`code/graphs/`)

- One PNG per trial showing the reference line on the graph and mouse trace during the task

### Trial Metrics Summary (`code/export/trial_metrics_summary.csv`)

Per-trial metrics using the **Cartesian normal** (perpendicular distance) from the participant's cursor to the target curve:

| Column         | Description                                |
|----------------|--------------------------------------------|
| participant    | Participant ID                             |
| mode           | no-feedback, numerical, or spatial-color   |
| trial          | Trial number                               |
| max_offset_px  | Maximum perpendicular offset from target   |
| min_offset_px  | Minimum perpendicular offset (closest)     |
| avg_offset_px  | Mean perpendicular offset                  |
| area_off_px2   | Total area enclosed between trace and curve |
| score          | 100 − (2 × avg_offset); higher = better   |

### Flat CSVs (`code/export/`)

- Per-trial files: `{participant}_{filename}_flat.csv`
- Combined file: `all_trials_cursor_and_target.csv`

**Columns:**

| Column       | Description                           |
|--------------|---------------------------------------|
| participant  | Participant ID                        |
| mode         | no-feedback, numerical, or spatial-color |
| trial        | Trial number                          |
| index        | Point index along the curve           |
| target_x     | Target x (graph/reference)            |
| target_y     | Target y (graph/reference)            |
| user_x       | Cursor x (participant)                |
| user_y       | Cursor y (participant)                |
| offset_px    | Perpendicular distance (error in px)  |

## How to Run

```bash
# From project root
pip install -r requirements.txt
python3 code/visualize_data.py
```

Add new CSVs to `code/data/` (under participant subfolders) and run again to regenerate graphs and exports.
