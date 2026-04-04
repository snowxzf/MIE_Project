#!/usr/bin/env python3
"""
Visualize Line Tracer CSV data: target curve vs participant cursor path.
Reads multi-section CSV files and generates overlay graphs.
"""

import csv
import re
from pathlib import Path
from typing import Any, Optional, Tuple

import matplotlib
matplotlib.use("Agg")  # headless backend for saving files
import matplotlib.pyplot as plt
import pandas as pd


# Normalized survey name (lower, collapsed spaces) -> trial folder / participant id used in trace data
SURVEY_NAME_TO_PARTICIPANT: dict[str, str] = {
    "soham shorey": "soham",
    "jia qi (eric) huang": "eric",
    "srishti krishnan": "srishti",
    "sumedhaa": "sumedhaa",
    "han": "han",
    "pranav upreti": "pranav",
    "mikeli italiano": "mikeli",
    "alison lei": "alison",
    "derek chen": "Derek Chen",
    "mani majd": "mani",
    "rachel liu": "rachel",
    "ines de uriarte alvarez de espejo": "Ines",
    "yiran feng": "yiran",
    "jessica chen": "j chen",
    "nareen kouyoumdjian": "Nareen",
    "lawrence ding": "lawrence",
    "aayush m": "Aayush",
    "selina liu": "selina",
    "rahi": "Rahi",
    "katherine betz": "katherine",
    "vanessa": "vanessa",
    "jadon tsai": "jadon t",
    "shiv kanade": "shiv",
    "robyn": "robyn",
    "victoria sun": "victoria",
    "bruce qiu": "bruce",
    "heidi ma": "heidi",
    "emily ye": "emily",
    "david": "david",
    "belinda": "belinda",
    "michael": "Michael",
}


def _norm_survey_name(name: str) -> str:
    s = (name or "").strip().lower()
    s = re.sub(r"\s+", " ", s)
    return s


def map_survey_name_to_participant(raw_name: str) -> Optional[str]:
    """Match a survey full name to the `participant` string used in data folders / trial CSVs."""
    key = _norm_survey_name(raw_name)
    if key in SURVEY_NAME_TO_PARTICIPANT:
        return SURVEY_NAME_TO_PARTICIPANT[key]
    # single-token first name (e.g. "david")
    tok = key.split()[0] if key else ""
    if not tok:
        return None
    hits = [tid for tid in SURVEY_NAME_TO_PARTICIPANT.values() if _norm_survey_name(tid).split()[0] == tok]
    return hits[0] if len(hits) == 1 else None


def _find_col(columns: list[str], *substrings: str) -> Optional[str]:
    subs = [s.lower() for s in substrings]
    for c in columns:
        cl = str(c).strip().lower()
        if all(s in cl for s in subs):
            return c
    return None


def parse_gaming_hours_per_day(hours_cell: Any) -> Tuple[Optional[float], str]:
    """Map survey gaming text to a numeric typical hours/day; return (value, raw text)."""
    raw = "" if hours_cell is None or (isinstance(hours_cell, float) and pd.isna(hours_cell)) else str(hours_cell).strip()
    if not raw:
        return None, raw
    t = raw.lower()
    if "0 hours" in t or "never/very rarely" in t:
        return 0.0, raw
    if "1-2 hours" in t:
        return 1.5, raw
    if "3-5 hours" in t:
        return 4.0, raw
    return None, raw


def normalize_gender_label(g: Any) -> Optional[str]:
    if g is None or (isinstance(g, float) and pd.isna(g)):
        return None
    s = str(g).strip()
    if not s:
        return None
    low = s.lower()
    if low in ("man", "male", "m"):
        return "man"
    if low in ("woman", "female", "f", "w"):
        return "woman"
    return low


def parse_mie_participant_survey(survey_path: Path) -> pd.DataFrame:
    """
    Read MIE_Participant_Data.csv (Microsoft Forms / survey export) and return
    one row per participant id used in trial CSVs, with standardized column names.
    """
    raw = pd.read_csv(survey_path, dtype=str, keep_default_na=False, encoding="utf-8-sig")
    raw.columns = [str(c).strip() for c in raw.columns]

    c_name = _find_col(list(raw.columns), "what is your name")
    c_age = _find_col(list(raw.columns), "what is your age")
    c_bio = _find_col(list(raw.columns), "biological sex")
    c_gender = _find_col(list(raw.columns), "what is your gender")
    c_major = _find_col(list(raw.columns), "engineering major")
    c_cb = _find_col(list(raw.columns), "colorblind")
    c_motor = _find_col(list(raw.columns), "fine motor")
    c_game = _find_col(list(raw.columns), "video games", "typical day")
    c_pc = _find_col(list(raw.columns), "familiar", "computers")
    c_timer = _find_col(list(raw.columns), "timer", "10 seconds")

    if not c_name:
        raise ValueError(f"Could not find name column in {survey_path}")

    rows: list[dict[str, Any]] = []
    for _, r in raw.iterrows():
        pid = map_survey_name_to_participant(str(r.get(c_name, "")))
        if not pid:
            continue
        hours_val, hours_raw = parse_gaming_hours_per_day(r.get(c_game, ""))
        age_val: Optional[int] = None
        try:
            age_val = int(str(r.get(c_age, "")).strip())
        except (ValueError, TypeError):
            pass
        comp_raw = str(r.get(c_pc, "")).strip() if c_pc else ""
        timer_raw = str(r.get(c_timer, "")).strip() if c_timer else ""
        comp_num: Optional[float] = None
        if comp_raw.isdigit():
            comp_num = float(comp_raw)
        stress_num: Optional[float] = None
        if timer_raw.isdigit():
            stress_num = float(timer_raw)

        rows.append(
            {
                "participant": pid,
                "gender": normalize_gender_label(r.get(c_gender, "")) if c_gender else None,
                "avg_gaming_hours_per_day": hours_val,
                "gaming_hours_survey_text": hours_raw,
                "age": age_val,
                "biological_sex": str(r.get(c_bio, "")).strip() if c_bio else "",
                "engineering_major": str(r.get(c_major, "")).strip() if c_major else "",
                "colorblind": str(r.get(c_cb, "")).strip() if c_cb else "",
                "fine_motor_difficulty": str(r.get(c_motor, "")).strip() if c_motor else "",
                "computer_familiarity_raw": comp_raw,
                "computer_familiarity_1to5": comp_num,
                "timer_stress_raw": timer_raw,
                "timer_stress_1to5": stress_num,
            }
        )

    if not rows:
        raise ValueError(f"No recognizable participants in {survey_path}")

    out = pd.DataFrame(rows)
    out = out.drop_duplicates(subset=["participant"], keep="last")
    return out


def sync_participant_demographics_from_survey(proj_dir: Path) -> Optional[Path]:
    """Write participant_demographics.csv from MIE_Participant_Data.csv when present."""
    survey = proj_dir / "MIE_Participant_Data.csv"
    if not survey.exists():
        return None
    df = parse_mie_participant_survey(survey)
    out = proj_dir / "participant_demographics.csv"
    df.to_csv(out, index=False, encoding="utf-8")
    return out


def parse_trial_csv(csv_path: Path) -> dict:
    """Parse a Line Tracer CSV with target_curve, user_path, and matched_data sections."""
    with open(csv_path, newline="", encoding="utf-8") as f:
        lines = list(csv.reader(f))

    result = {
        "trial": None,
        "timestamp": None,
        "mode": None,
        "gender": None,
        "avg_gaming_hours_per_day": None,
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
        elif row[0] == "gender" and len(row) > 1:
            g = (row[1] or "").strip()
            result["gender"] = g if g else None
        elif row[0] == "avg_gaming_hours_per_day" and len(row) > 1:
            try:
                result["avg_gaming_hours_per_day"] = float(str(row[1]).replace(",", "."))
            except (ValueError, TypeError):
                result["avg_gaming_hours_per_day"] = None

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
    preferred = [
        "participant",
        "gender",
        "avg_gaming_hours_per_day",
        "gaming_hours_survey_text",
        "age",
        "mode",
        "trial",
        "max_offset_px",
        "min_offset_px",
        "avg_offset_px",
        "area_off_px2",
        "score",
    ]
    cols = [c for c in preferred if c in df.columns]
    for c in sorted(df.columns):
        if c not in cols:
            cols.append(c)

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
            elif c == "avg_gaming_hours_per_day" and isinstance(val, (int, float)) and not pd.isna(val):
                val = f"{val:g}"
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


def load_participant_demographics(proj_dir: Path) -> Optional[pd.DataFrame]:
    """Prefer live MIE_Participant_Data.csv; else standardized participant_demographics.csv."""
    survey = proj_dir / "MIE_Participant_Data.csv"
    if survey.exists():
        return parse_mie_participant_survey(survey)
    p = proj_dir / "participant_demographics.csv"
    if not p.exists():
        return None
    d = pd.read_csv(p, keep_default_na=False, encoding="utf-8")
    d.columns = [str(c).strip() for c in d.columns]
    if "participant" not in d.columns:
        return None
    d["participant"] = d["participant"].astype(str).str.strip()
    if "gender" in d.columns:
        d["gender"] = d["gender"].astype(str).str.strip().replace({"": pd.NA})
    for num_col in ("avg_gaming_hours_per_day", "age", "computer_familiarity_1to5", "timer_stress_1to5"):
        if num_col in d.columns:
            d[num_col] = pd.to_numeric(
                d[num_col].astype(str).str.replace(",", ".", regex=False),
                errors="coerce",
            )
    return d.drop_duplicates(subset=["participant"], keep="first")


def merge_metrics_with_demographics(metrics_df: pd.DataFrame, demo: Optional[pd.DataFrame]) -> pd.DataFrame:
    """Merge survey/demographics; trial CSV metadata overrides same-named fields when set."""
    if demo is None:
        return metrics_df

    out = metrics_df.merge(demo, on="participant", how="left", suffixes=("_trial", "_survey"))
    demo_only_cols = [c for c in demo.columns if c != "participant"]
    numeric_survey = {
        "avg_gaming_hours_per_day",
        "age",
        "computer_familiarity_1to5",
        "timer_stress_1to5",
    }

    for col in demo_only_cols:
        t, s = f"{col}_trial", f"{col}_survey"
        if t in out.columns and s in out.columns:
            if col == "gender":
                gt = out[t].astype(str).str.strip()
                bad = gt.isin(("", "nan", "None", "<NA>"))
                out[col] = gt.where(~bad, pd.NA).fillna(out[s])
            elif col in numeric_survey:
                lt = pd.to_numeric(out[t], errors="coerce")
                rs = pd.to_numeric(out[s], errors="coerce")
                out[col] = lt.where(lt.notna(), rs)
            else:
                lt = out[t].astype(str).str.strip()
                bad = lt.isin(("", "nan", "None", "<NA>"))
                out[col] = out[t].where(~bad, out[s])
            out = out.drop(columns=[t, s])
        elif t in out.columns and t != col:
            out = out.rename(columns={t: col})
        elif s in out.columns and s != col:
            out = out.rename(columns={s: col})

    return out


def export_flat_csv(data: dict, csv_path: Path, output_path: Path) -> None:
    """
    Export a flat CSV with columns for cursor data and target data.
    Uses matched_data for aligned rows. Optional trial metadata (gender, hours gaming)
    is duplicated on every row so combined flat exports can be analyzed without a join.
    """
    matched = data["matched_data"]
    if not matched:
        return

    participant = csv_path.parent.name
    df = pd.DataFrame(matched)
    df.insert(0, "participant", participant)
    df.insert(1, "mode", data["mode"])
    df.insert(2, "trial", data["trial"])
    if data.get("gender"):
        df["gender"] = data["gender"]
    if data.get("avg_gaming_hours_per_day") is not None:
        df["avg_gaming_hours_per_day"] = data["avg_gaming_hours_per_day"]
    df.to_csv(output_path, index=False)
    print(f"  exported: {output_path.name}")


def main():
    data_dir = Path(__file__).parent / "data"
    output_dir = Path(__file__).parent / "graphs"
    export_dir = Path(__file__).parent / "export"
    proj_dir = Path(__file__).parent
    output_dir.mkdir(exist_ok=True)
    export_dir.mkdir(exist_ok=True)
    synced = sync_participant_demographics_from_survey(proj_dir)
    if synced:
        print(f"Synced survey → {synced.name}")
    demo = load_participant_demographics(proj_dir)

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
            rec = {
                "participant": p.parent.name,
                "mode": data["mode"],
                "trial": data["trial"],
                "timestamp": data.get("timestamp", ""),
                "filename": p.stem,
                **m,
            }
            if data.get("gender"):
                rec["gender"] = data["gender"]
            if data.get("avg_gaming_hours_per_day") is not None:
                rec["avg_gaming_hours_per_day"] = data["avg_gaming_hours_per_day"]
            metrics_rows.append(rec)

    # Export per-participant trial metrics summary
    if metrics_rows:
        metrics_df = pd.DataFrame(metrics_rows)
        metrics_df = merge_metrics_with_demographics(metrics_df, demo)
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
