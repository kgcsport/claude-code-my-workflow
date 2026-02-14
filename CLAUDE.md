# CLAUDE.MD -- Academic Project Development with Claude Code

<!-- Economics Teaching Apps at Vassar College.
     Shiny app sandbox for classroom tools.
     Slide infrastructure (Beamer/Quarto) is preserved but deprioritized —
     slide-specific skills and agents remain functional if needed. -->

**Project:** Economics Teaching Apps
**Institution:** Vassar College
**Branch:** main

---

## Core Principles

- **Plan first** -- enter plan mode before non-trivial tasks; save plans to `quality_reports/plans/`
- **Verify after** -- run apps and confirm output at the end of every task
- **Quality gates** -- nothing ships below 80/100
- **[LEARN] tags** -- when corrected, save `[LEARN:category] wrong → right` to MEMORY.md

---

## Folder Structure

```
economics-teaching-apps/
├── CLAUDE.MD                    # This file
├── .claude/                     # Rules, skills, agents, hooks
├── apps/                        # Shiny apps (each in own subfolder)
├── Bibliography_base.bib        # Centralized bibliography
├── Figures/                     # Figures and images
├── Preambles/header.tex         # LaTeX headers
├── Slides/                      # Beamer .tex files (deprioritized)
├── Quarto/                      # RevealJS .qmd files (deprioritized)
├── docs/                        # GitHub Pages (auto-generated)
├── scripts/                     # Utility scripts + R code
├── quality_reports/             # Plans, session logs, merge reports
├── explorations/                # Research sandbox (see rules)
├── templates/                   # Session log, quality report templates
└── master_supporting_docs/      # Papers and references
```

---

## Commands

```bash
# Run a Shiny app locally
Rscript -e "shiny::runApp('apps/app-name')"

# Run Shiny app tests
Rscript -e "shinytest2::test_app('apps/app-name')"

# Run all R tests
Rscript -e "testthat::test_dir('tests')"

# LaTeX (3-pass, XeLaTeX only — for slide work)
cd Slides && TEXINPUTS=../Preambles:$TEXINPUTS xelatex -interaction=nonstopmode file.tex
BIBINPUTS=..:$BIBINPUTS bibtex file
TEXINPUTS=../Preambles:$TEXINPUTS xelatex -interaction=nonstopmode file.tex
TEXINPUTS=../Preambles:$TEXINPUTS xelatex -interaction=nonstopmode file.tex

# Deploy Quarto to GitHub Pages
./scripts/sync_to_docs.sh LectureN

# Quality score
python scripts/quality_score.py Quarto/file.qmd
```

---

## Quality Thresholds

| Score | Gate | Meaning |
|-------|------|---------|
| 80 | Commit | Good enough to save |
| 90 | PR | Ready for deployment |
| 95 | Excellence | Aspirational |

---

## Skills Quick Reference

| Command | What It Does |
|---------|-------------|
| `/compile-latex [file]` | 3-pass XeLaTeX + bibtex |
| `/deploy [LectureN]` | Render Quarto + sync to docs/ |
| `/extract-tikz [LectureN]` | TikZ → PDF → SVG |
| `/proofread [file]` | Grammar/typo/overflow review |
| `/visual-audit [file]` | Slide layout audit |
| `/pedagogy-review [file]` | Narrative, notation, pacing review |
| `/review-r [file]` | R code quality review |
| `/qa-quarto [LectureN]` | Adversarial Quarto vs Beamer QA |
| `/slide-excellence [file]` | Combined multi-agent review |
| `/translate-to-quarto [file]` | Beamer → Quarto translation |
| `/validate-bib` | Cross-reference citations |
| `/devils-advocate` | Challenge slide design |
| `/create-lecture` | Full lecture creation |
| `/commit [msg]` | Stage, commit, PR, merge |
| `/lit-review [topic]` | Literature search + synthesis |
| `/research-ideation [topic]` | Research questions + strategies |
| `/interview-me [topic]` | Interactive research interview |
| `/review-paper [file]` | Manuscript review |
| `/data-analysis [dataset]` | End-to-end R analysis |

---

## Shiny App Patterns

| Pattern | Description | When to Use |
|---------|-------------|-------------|
| | | |

<!-- Add patterns as apps are built, e.g.:
| Single-file app | app.R with inline UI/server | Simple demos |
| Modular app | R/mod_*.R modules | Multi-panel apps |
| Dashboard | shinydashboard layout | Data exploration tools |
-->

## Common UI Components

| Component | Package | Use Case |
|-----------|---------|----------|
| | | |

<!-- Add components as they emerge, e.g.:
| plotOutput + renderPlot | shiny | Static ggplot visualizations |
| plotlyOutput + renderPlotly | plotly | Interactive charts |
| DT::dataTableOutput | DT | Sortable data tables |
-->

---

## Current Project State

| App | Folder | Status | Key Functionality |
|-----|--------|--------|-------------------|
| | | | |

<!-- Track apps as they are built, e.g.:
| Participation Tracker | apps/participation | In progress | Real-time attendance + engagement |
| Supply & Demand | apps/supply-demand | Planned | Interactive equilibrium visualization |
-->
