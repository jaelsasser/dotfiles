# Runbook & friends

Operational procedure read under time pressure, often mid-incident (friends: playbooks, incident response). Front-load preconditions; end with rollback and troubleshooting.

```markdown
# Runbook: <operation>

## Preconditions
<Access, state, and checks that must hold first.>

## Steps
1. <Verb + command + one-line why.>
2. …

## Rollback
<How to undo a failed step.>

## Troubleshooting
### `<distinctive line of the error>`
<Cause, then fix.>
```
