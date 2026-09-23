# Archived event-study scripts

Superseded by the [code/matching/](../../matching) pipeline. Kept for reference — the new stages port
their logic, and the comments in these files record hard-won operational lessons (memory limits,
fork behaviour, checkpointing) that are still accurate.

| file | superseded by | why |
|---|---|---|
| `eligibility_from_annual_panel.R` | `1_build_universe.R` | Built eligibility on the annual Virk panel, which **ends in 2018** — most competitive award events are 2019+. It also could not run: `ev` is integer (`.I`, `:48`) but is filtered with `%chin%` (`:68`, `:80`, `:81`), which errors. The `.rds` files it appears to have produced came from an earlier version. |
| `find_control_firms.R` | `2_match_controls.R` | First-generation matcher. Its **selection rule is preserved verbatim** in the new stage 2: `control_qscore = sqrt(mean(fte_diff_sq)) / mean(fte_treatment)`, keeping every control tied at the minimum (`:263`; the `mean_fte_diff_sq` variant at `:262` is commented out). Note its header claim of matching on "FTE and age" is wrong — `firm_age` was only carried into the output, never scored. |
| `find_control_firms_never_winners.R` | `2_match_controls.R` | Second-generation matcher. Its 6-rung industry/kommune cascade (`:246-253`) and balanced ±h test (`:203-209`) carry over, with two changes: the winner universe now comes from the combined dataset (this one used OT+KFST only, missing ~20% of winning firms), and a rung must yield `MATCH_MIN_RUNG` eligible candidates rather than merely one. |
| `build_event_study_panel.R` | `3_build_reg_data.R` | Chunked match-table × panel join. Logic carries over; the new stage fixes `cut(x, 1)` erroring on a single chunk and drops the hardcoded `chunk_events = 100` that silently overrode `ESP_CHUNK_EVENTS`. |

Also note `6a_estudy_control.Rmd:336` has `source("R/code/analysis/find_control_firms.R")` — that path
has no `R/` directory at the project root, so it was already broken before this move.
