# Project instructions

Keep `docs/manual.md` synchronized with user-visible behavior. When changing CLI
commands, options, defaults, supported imports, configuration or the workflow,
update the manual and relevant Cmdliner help in the same change. Update README
examples when affected. Verify documented commands against the CLI before
considering the work complete.

Keep `docs/plan.md` and `CHANGES.md` current in the same commit as the change.
`docs/plan.md` says where the project stands and what comes next: tick off
finished items, add work you discover, refresh its figures when they move, and
update its date. `CHANGES.md` records what changed, newest first under
Unreleased, with an **Action required** note whenever users must act, such as
rebuilding the database after a schema change.

Use Base for general utilities, decimal for quantities, Ptime for dates and
clocks, and Bos for high-level OS operations. Keep accounting arithmetic exact
through Quantity's local contexts. Do not reintroduce Core, Core_unix,
Bigdecimal or a home-grown standard-library compatibility layer.
