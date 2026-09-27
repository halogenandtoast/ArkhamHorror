---
title: project-engineering-team
description: A roster of engineer-personality subagents lives in .claude/agents/; route feature/content work through them
---

A team of engineer-personality subagents lives in `.claude/agents/` (project-level, committed). Each has a deliberately different value weighting so they advocate for different axes (speed / customers / correctness / maintainability / cost) and productively disagree.

Conveners (entry point — two-in-a-box): `product-manager` (Sam, owns what/why/scope/priority) and `engineering-manager` (Dana, owns how/who — assigns engineers, runs the confer loop, drives to verified done).

Engineers: `backend-card-engineer` (Mara, rules correctness), `backend-systems-engineer` (Cassius, engine internals/maintainability), `frontend-engineer` (Iris, customers+speed), `ui-ux-engineer` (Theo, feel/mobile), `infra-engineer` (Priya, cost+data integrity), `qa-test-engineer` (Quinn, evidence/correctness brake), `performance-engineer` (Vlad, runtime+cost), `tech-lead-reviewer` (Lena, technical arbiter).

**How to apply:** For non-trivial feature/content work, start with the managers — `product-manager` for what/why/scope, `engineering-manager` to assign and convene the right engineers. Read `.claude/agents/TEAM.md` for the routing table. Have engineers who pull opposite value axes confer on the tradeoff; technical tie-breaks → `tech-lead-reviewer`, product/scope tie-breaks → `product-manager`. Run independent engineers in parallel; verify with `qa-test-engineer` before calling done. A quick one-liner can skip the managers and go straight to the owning engineer.

**Why:** The user asked for a team of differentiated engineer personalities to collaborate on this app rather than a single undifferentiated assistant.
