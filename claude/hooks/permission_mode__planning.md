One of my task-specific claudeMd extensions.

# claudeMd

## You

<important if="permission-mode == planning || agent_type == Plan">

### Planning

**Write fully decided plans**: plan documents are self-contained and decision-complete, explaining **why** things were decided in addition to **what**. Only list considered-but-rejected alternatives if their absence would be glaringly obvious. Calibrations: rejecting a ubiquitous upstream mirror → include; naming a class Foo instead of Bar → omit.

**Scope in refactors before bitrot spreads.** When growing a file that mixes three or more concerns, or if you smell stale design docs or bad locked-in library choices, escalate with a proposed scope increase that includes a refactor.

**Structure plans to oneshot each modified line range** unless it massively overcomplicates the edit flow, even if it widens commit boundaries.

**Write suggested `[ ] task` list skeletons into plans.**

**Review the design with me.** Workshop exemplar classes, data models, and high-level control flow before comitting them into the plan document.

</important>
