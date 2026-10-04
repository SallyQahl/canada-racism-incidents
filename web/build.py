"""
Build the standalone web dashboard (docs/index.html) from the cleaned data.

Run from the repository root, after R/01_clean.R:
    python3 web/build.py

The output is one self-contained HTML file (data embedded), so it can be hosted
for free on GitHub Pages: Settings -> Pages -> Deploy from branch -> /docs.
"""
import json
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
POP = pd.read_csv(ROOT / "data/reference/province_population.csv")
NAME_FIX = {"Yukon Territory": "Yukon", "Québec": "Quebec"}

read = lambda p: pd.read_csv(ROOT / p, keep_default_na=False, na_values=["NA"])
incidents = read("data/processed/incidents_clean.csv")
labels = read("data/processed/labels_long.csv")

# ---- multi-label fields -> integer codes --------------------------------------
fields = {"Type of incident": "t", "Context": "c", "Identity-based": "d", "Targeted community": "g"}
dicts = {k: sorted(labels.loc[labels.field == f, "short"].unique()) for f, k in fields.items()}
codes = {}
for f, k in fields.items():
    for iid, grp in labels[labels.field == f].groupby("incident_id"):
        codes.setdefault(iid, {})[k] = sorted({dicts[k].index(s) for s in grp.short})

cats = sorted(incidents.category.unique())
provs = sorted(incidents.province.unique())
rows = []
for r in incidents.itertuples():
    c = codes.get(r.incident_id, {})
    rows.append([
        None if pd.isna(r.date) else r.date, provs.index(r.province), r.city, cats.index(r.category),
        c.get("t", []), c.get("c", []), c.get("d", []), c.get("g", []),
        "" if pd.isna(r.description) else r.description, r.url,
    ])

# ---- province boundaries: round, dedupe, rewind for d3 -------------------------
def signed_area(ring):
    return sum(x1 * y2 - x2 * y1 for (x1, y1), (x2, y2) in zip(ring, ring[1:] + ring[:1])) / 2

def clean_ring(ring, exterior):
    out = []
    for x, y in ring:
        p = [round(x, 3), round(y, 3)]
        if not out or out[-1] != p:
            out.append(p)
    if len(out) < 4:
        return None
    # d3-geo expects exterior rings clockwise (negative planar area), holes counter-clockwise
    if (signed_area(out) > 0) == exterior:
        out.reverse()
    return out

def clean_polygon(poly):
    rings = [clean_ring(r, i == 0) for i, r in enumerate(poly)]
    return None if rings[0] is None else [r for r in rings if r]

geo = json.loads((ROOT / "data/reference/canada_provinces.geojson").read_text())
pop = dict(zip(POP.province, POP.population_2021))
features = []
for ft in geo["features"]:
    name = NAME_FIX.get(ft["properties"]["name"], ft["properties"]["name"])
    g = ft["geometry"]
    polys = g["coordinates"] if g["type"] == "MultiPolygon" else [g["coordinates"]]
    polys = [p for p in (clean_polygon(p) for p in polys) if p]
    features.append({"type": "Feature", "properties": {"name": name, "pop": int(pop[name])},
                     "geometry": {"type": "MultiPolygon", "coordinates": polys}})

payload = {"provs": provs, "cats": cats, "dicts": dicts, "rows": rows,
           "geo": {"type": "FeatureCollection", "features": features}}
data = json.dumps(payload, separators=(",", ":"), ensure_ascii=False).replace("</", "<\\/")

template = (ROOT / "web/template.html").read_text()
html = template.replace("__DATA__", data)
page = ("<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, viewport-fit=cover\">\n"
        "</head>\n<body>\n" + html + "\n</body>\n</html>\n")

(ROOT / "docs").mkdir(exist_ok=True)
(ROOT / "docs/index.html").write_text(page)          # GitHub Pages version (full document)
(ROOT / "web/dashboard_artifact.html").write_text(html)  # body-only version
print(f"docs/index.html: {len(page) / 1024:.0f} KB, {len(rows)} incidents, {len(features)} provinces")
