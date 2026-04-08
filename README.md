# reach.viz

**Flode module:** Visualisation — standardised ggplot2-compatible chart themes
and flood-specific plot functions.

Part of the [Flode](https://github.com/JonPayneEA) R package ecosystem for the
Environment Agency's Forecasting and Warning team.

---

## Installation

```r
# From the team's internal package registry, or directly from source:
devtools::install_local("path/to/reach.viz")

# Suggested dependencies for model cascade diagrams:
install.packages(c("ggraph", "igraph"))
```

Dependencies are pinned via `renv`. Run `renv::restore()` after cloning.

---

## What this module provides

### Theme and scales

```r
library(reach.viz)
library(ggplot2)

# Team ggplot2 theme
ggplot(df, aes(x, y)) + geom_line() + theme_flood()

# QC flag colour scale (integer 1–6)
ggplot(dt, aes(timestamp, qc_value, colour = factor(qc_flag))) +
  geom_line() +
  scale_colour_qc()

# Condition rating scale
ggplot(dt, aes(model, score, colour = condition_rating)) +
  geom_point() +
  scale_colour_condition()

# Tier scale
ggplot(dt, aes(model, score, colour = tier)) +
  geom_point() +
  scale_colour_tier()
```

### Hydrometric time series

```r
dt <- reach.io::read_silver("EDEN_39001")

# Time series with QC flag colour encoding
plot_hydrograph(dt)
plot_hydrograph(dt, site_col = "site_id", show_raw = TRUE)

# Observed vs. simulated overlay
plot_obs_sim_ts(wide_dt)

# Flow duration curve
plot_flow_duration(dt, group_col = "site_id")
```

### QC overview

```r
# Flag distribution heatmap (month × site)
plot_qc_heatmap(dt, period = "month")

# Data availability stacked bar
plot_data_availability(dt)
```

### Ensemble forecasts

```r
ens_dt <- reach.ensemble::get_forecast_members("EDEN_39001")

plot_ensemble_ribbon(ens_dt)
plot_ensemble_spaghetti(ens_dt, highlight_median = TRUE)
plot_exceedance_probability(ens_dt, thresholds = c(Alert = 50, Warning = 100))
```

### Model performance and validation

```r
plot_obs_sim_scatter(dt, colour_by = "period")
plot_residuals(dt)
plot_performance_metrics(dt, thresholds = c(NSE = 0.6, KGE = 0.6))
```

### Condition assessment

```r
# Radar chart: 5-factor breakdown
plot_condition_factors(condition_dt)

# Condition register overview (all models, sorted by score)
plot_condition_register(register_dt)
```

### Model cascade diagram

Requires `ggraph` and `igraph` (in Suggests, not installed by default):

```r
install.packages(c("ggraph", "igraph"))
plot_model_cascade(connections_dt, models_dt)
```

---

## Input schemas

All plot functions accept `data.table` objects. Key schemas:

| Function | Required columns |
|---|---|
| `plot_hydrograph()` | `timestamp`, `qc_value`, `qc_flag` |
| `plot_obs_sim_ts()` | `timestamp`, `observed`, `simulated` |
| `plot_flow_duration()` | `qc_value`, `qc_flag` |
| `plot_qc_heatmap()` | `timestamp`, `qc_flag`, `site_id` |
| `plot_ensemble_ribbon()` | `timestamp`, `value`, `ensemble_member` |
| `plot_exceedance_probability()` | `value`, `ensemble_member` |
| `plot_obs_sim_scatter()` | `observed`, `simulated` |
| `plot_residuals()` | `timestamp`, `residual` |
| `plot_performance_metrics()` | `metric`, `value`, `site_id` |
| `plot_condition_factors()` | `model_id`, `factor`, `score` |
| `plot_condition_register()` | `model_id`, `condition_score`, `condition_rating`, `tier`, `status` |
| `plot_model_cascade()` | edges: `upstream_model`, `downstream_model`, `connection_type`, `status`; nodes: `model_id`, `tier`, `condition_score`, `status` |

---

## Development status

**Tier 1 (Experimental).** This module enters the Flode ecosystem as a
research/prototype. Promotion to Tier 2 (Analytical) requires peer review and
confirmed ≥70% test coverage via `covr`. Promotion to Tier 3 (Operational)
requires Owner sign-off.

```r
# Run tests
devtools::test()

# Check coverage
covr::package_coverage()

# Lint
lintr::lint_package()

# Full R CMD check
devtools::check()
```

---

## Governance

- Parent document: Data and Digital Asset Governance Framework v2.0
- Module: reach.viz within Flode
- Review date: 2027-03-31
- Proposed to Flode Steward via GitHub issue before merge into Flode NAMESPACE
