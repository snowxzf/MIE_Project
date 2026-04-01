#!/usr/bin/env python3
"""
Visualize Line Tracer CSV data: target curve vs participant cursor path.
Reads multi-section CSV files and generates overlay graphs.
"""

import csv
from pathlib import Path
from typing import Optional

import matplotlib
matplotlib.use("Agg")  # headless backend for saving files
import matplotlib.pyplot as plt
import pandas as pd


def parse_trial_csv(csv_path: Path) -> dict:
    """Parse a Line Tracer CSV with target_curve, user_path, and matched_data sections."""
    with open(csv_path, newline="", encoding="utf-8") as f:
        lines = list(csv.reader(f))

    result = {
        "trial": None,
        "timestamp": None,
        "mode": None,
        "target_curve": [],
        "user_path": [],
        "matched_data": [],
    }

    i = 0
    while i < len(lines):
        row = lines[i]
        if not row:
            i += 1
            continue

        # Metadata
        if row[0] == "trial" and len(row) > 1:
            result["trial"] = int(row[1])
        elif row[0] == "timestamp" and len(row) > 1:
            result["timestamp"] = row[1]
        elif row[0] == "mode" and len(row) > 1:
            result["mode"] = row[1]

        # target_curve: index,target_x,target_y
        if row[0] == "target_curve" and i + 1 < len(lines):
            header = lines[i + 1]
            if "target_x" in str(header) and "target_y" in str(header):
                i += 2
                while i < len(lines) and lines[i] and len(lines[i]) >= 3:
                    try:
                        idx, tx, ty = int(lines[i][0]), float(lines[i][1]), float(lines[i][2])
                        result["target_curve"].append({"index": idx, "target_x": tx, "target_y": ty})
                        i += 1
                    except (ValueError, IndexError):
                        break
                continue

        # user_path: index,user_x,user_y
        if row[0] == "user_path" and i + 1 < len(lines):
            header = lines[i + 1]
            if "user_x" in str(header) and "user_y" in str(header):
                i += 2
                while i < len(lines) and lines[i] and len(lines[i]) >= 3:
                    try:
                        idx, ux, uy = int(lines[i][0]), float(lines[i][1]), float(lines[i][2])
                        result["user_path"].append({"index": idx, "user_x": ux, "user_y": uy})
                        i += 1
                    except (ValueError, IndexError):
                        break
                continue

        # matched_data: index,target_x,target_y,user_x,user_y,offset_px
        if row[0] == "matched_data" and i + 1 < len(lines):
            header = lines[i + 1]
            if "offset_px" in str(header):
                i += 2
                while i < len(lines) and lines[i] and len(lines[i]) >= 6:
                    try:
                        row_vals = lines[i]
                        result["matched_data"].append({
                            "index": int(row_vals[0]),
                            "target_x": float(row_vals[1]),
                            "target_y": float(row_vals[2]),
                            "user_x": float(row_vals[3]),
                            "user_y": float(row_vals[4]),
                            "offset_px": float(row_vals[5]),
                        })
                        i += 1
                    except (ValueError, IndexError):
                        break
                continue

        i += 1

    return result


def plot_trial(data: dict, csv_path: Path, output_dir: Path) -> Optional[Path]:
    """Plot target curve vs participant cursor path for one trial."""
    target = data["target_curve"]
    user = data["user_path"]

    if not target or not user:
        print(f"  [skip] insufficient data: {csv_path.name}")
        return None

    target_x = [p["target_x"] for p in target]
    target_y = [p["target_y"] for p in target]
    user_x = [p["user_x"] for p in user]
    user_y = [p["user_y"] for p in user]

    participant = csv_path.parent.name
    filename = csv_path.stem

    fig, ax = plt.subplots(figsize=(9, 5))
    ax.plot(target_x, target_y, color="#4a36c0", linewidth=2, label="Target curve (graph)")
    ax.plot(user_x, user_y, color="#10a052", linewidth=1.8, alpha=0.85, label="Participant cursor")
    ax.set_xlim(0, 720)
    ax.set_ylim(380, 0)  # canvas coords: y increases downward
    ax.set_aspect("equal")
    ax.legend(loc="upper right")
    ax.set_title(f"{participant} — {data['mode']} (trial {data['trial']}) — {filename}")
    ax.set_xlabel("x (px)")
    ax.set_ylabel("y (px)")
    ax.grid(True, alpha=0.3)

    out_file = output_dir / f"{participant}_{filename}.png"
    fig.tight_layout()
    fig.savefig(out_file, dpi=120)
    plt.close()
    return out_file


def compute_trial_metrics(data: dict) -> dict | None:
    """
    Compute per-trial metrics using Cartesian normals (perpendicular distances)
    from the cursor to the target curve. Matches the Line Tracer website logic.
    """
    matched = data["matched_data"]
    if not matched or len(matched) < 2:
        return None

    offsets = [m["offset_px"] for m in matched]
    max_offset = max(offsets)
    min_offset = min(offsets)
    avg_offset = sum(offsets) / len(offsets)

    # Total area off: trapezoidal rule over target segments (matches index.html computeArea)
    def dist(ax, ay, bx, by):
        return ((ax - bx) ** 2 + (ay - by) ** 2) ** 0.5

    area_off = 0.0
    for i in range(1, len(matched)):
        prev = matched[i - 1]
        curr = matched[i]
        seg_len = dist(prev["target_x"], prev["target_y"], curr["target_x"], curr["target_y"])
        area_off += ((curr["offset_px"] + prev["offset_px"]) / 2) * seg_len

    score = max(0, round(100 - avg_offset * 2))

    return {
        "max_offset_px": round(max_offset, 2),
        "min_offset_px": round(min_offset, 2),
        "avg_offset_px": round(avg_offset, 2),
        "area_off_px2": round(area_off, 1),
        "score": score,
    }


def write_metrics_table_html(df: pd.DataFrame, output_path: Path) -> None:
    """Generate an HTML file with a styled table of trial metrics."""
    display_cols = ["participant", "mode", "trial", "max_offset_px", "min_offset_px", "avg_offset_px", "area_off_px2", "score"]
    cols = [c for c in display_cols if c in df.columns]

    def score_class(val):
        if pd.isna(val): return ""
        v = int(val)
        return "score-good" if v >= 98 else "score-meh" if v >= 95 else "score-low"

    rows_html = ""
    for _, row in df.iterrows():
        cells = []
        for c in cols:
            val = row.get(c, "")
            if c == "mode":
                val = str(val).replace("-", " ").title()
            elif c in ("max_offset_px", "min_offset_px", "avg_offset_px") and isinstance(val, (int, float)):
                val = f"{val:.2f}"
            elif c == "area_off_px2" and isinstance(val, (int, float)):
                val = f"{val:,.0f}"
            elif c == "score":
                score_val = row.get("score")
                score_cls = score_class(score_val)
                cells.append(f'<td class="{score_cls}">{int(score_val) if not pd.isna(score_val) else ""}</td>')
                continue
            cells.append(f"<td>{val}</td>")
        rows_html += f"<tr>{''.join(cells)}</tr>\n"

    def header_label(c):
        s = c.replace("_", " ").title()
        if "px2" in c: s = s.replace("Px2", "(px²)")
        elif "px" in c: s = s.replace("Px", "(px)")
        return s

    headers = [header_label(c) for c in cols]
    header_html = "".join(f"<th>{h}</th>" for h in headers)

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Trial Metrics — Line Tracer</title>
<style>
  * {{ margin: 0; padding: 0; box-sizing: border-box; }}
  body {{
    font-family: 'Segoe UI', system-ui, sans-serif;
    background: #f7f7fd;
    color: #1a1a2e;
    padding: 24px;
  }}
  h1 {{
    font-size: 1.25rem;
    font-weight: 600;
    letter-spacing: 0.06em;
    color: #3a3a5c;
    margin-bottom: 20px;
  }}
  .table-wrap {{
    overflow-x: auto;
    background: #fff;
    border-radius: 12px;
    box-shadow: 0 2px 16px rgba(0,0,0,0.08);
    border: 1px solid #e2e2f0;
  }}
  table {{
    width: 100%;
    border-collapse: collapse;
  }}
  th, td {{
    padding: 12px 16px;
    text-align: left;
  }}
  th {{
    background: #4a36c0;
    color: #fff;
    font-size: 0.72rem;
    font-weight: 600;
    letter-spacing: 0.06em;
    text-transform: uppercase;
  }}
  tr:nth-child(even) {{ background: #fafaff; }}
  tr:hover {{ background: #f0f0fa; }}
  td {{
    font-size: 0.9rem;
  }}
  .score-good {{ color: #18a050; font-weight: 700; }}
  .score-meh {{ color: #b07010; font-weight: 600; }}
  .score-low {{ color: #c83030; font-weight: 700; }}
</style>
</head>
<body>
<h1>Trial Metrics Summary — Cartesian Normal to Target Curve</h1>
<div class="table-wrap">
<table>
<thead><tr>{header_html}</tr></thead>
<tbody>
{rows_html}
</tbody>
</table>
</div>
</body>
</html>"""

    with open(output_path, "w", encoding="utf-8") as f:
        f.write(html)


def export_flat_csv(data: dict, csv_path: Path, output_path: Path) -> None:
    """
    Export a flat CSV with columns for cursor data and target data.
    Uses matched_data for aligned rows.
    """
    matched = data["matched_data"]
    if not matched:
        return

    participant = csv_path.parent.name
    df = pd.DataFrame(matched)
    df.insert(0, "participant", participant)
    df.insert(1, "mode", data["mode"])
    df.insert(2, "trial", data["trial"])
    df.to_csv(output_path, index=False)
    print(f"  exported: {output_path.name}")


def main():
    data_dir = Path(__file__).parent / "data"
    output_dir = Path(__file__).parent / "graphs"
    export_dir = Path(__file__).parent / "export"
    output_dir.mkdir(exist_ok=True)
    export_dir.mkdir(exist_ok=True)

    csv_files = sorted(data_dir.rglob("*.csv"))
    if not csv_files:
        print("No CSV files found in", data_dir)
        return

    print(f"Found {len(csv_files)} CSV files")
    metrics_rows = []

    for p in csv_files:
        data = parse_trial_csv(p)
        out = plot_trial(data, p, output_dir)
        if out:
            print(f"  graph: {out.name}")

        # Export flat CSV for this trial
        flat_name = f"{p.parent.name}_{p.stem}_flat.csv"
        export_flat_csv(data, p, export_dir / flat_name)

        # Compute per-trial metrics (cartesian normal to graph)
        m = compute_trial_metrics(data)
        if m:
            metrics_rows.append({
                "participant": p.parent.name,
                "mode": data["mode"],
                "trial": data["trial"],
                "timestamp": data.get("timestamp", ""),
                "filename": p.stem,
                **m,
            })

    # Export per-participant trial metrics summary
    if metrics_rows:
        metrics_df = pd.DataFrame(metrics_rows)
        metrics_path = export_dir / "trial_metrics_summary.csv"
        metrics_df.to_csv(metrics_path, index=False)
        print(f"\nTrial metrics summary: {metrics_path}")
        print("  Columns: participant, mode, trial, max_offset_px, min_offset_px, avg_offset_px, area_off_px2, score")

        # Generate visual HTML table
        html_path = export_dir / "trial_metrics_table.html"
        write_metrics_table_html(metrics_df, html_path)
        print(f"  Visual table: {html_path}")

    # Aggregate flat CSVs into one combined file
    flat_files = list(export_dir.glob("*_flat.csv"))
    if flat_files:
        dfs = []
        for f in flat_files:
            try:
                dfs.append(pd.read_csv(f))
            except Exception as e:
                print(f"  skip {f.name}: {e}")
        if dfs:
            combined = pd.concat(dfs, ignore_index=True)
            combined_path = export_dir / "all_trials_cursor_and_target.csv"
            combined.to_csv(combined_path, index=False)
            print(f"\nCombined flat CSV: {combined_path}")


if __name__ == "__main__":
    main()
