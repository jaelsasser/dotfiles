#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["jinja2"]
# ///
"""Render rubric.j2 against USER_CLAUDE.md and the local skills catalog."""

from pathlib import Path

import jinja2

HERE = Path(__file__).resolve().parent


def frontmatter(path):
    out, started = {}, False
    for line in path.read_text().splitlines():
        if line.strip() == "---":
            if started:
                break
            started = True
            continue
        if started:
            k, sep, v = line.partition(":")
            if sep:
                out[k.strip()] = v.strip()
    return out


def skills():
    skills_dir = HERE / "skills"
    if not skills_dir.is_dir():
        return []
    out = []
    for entry in sorted(skills_dir.iterdir()):
        skill_md = entry / "SKILL.md"
        if not skill_md.is_file():
            continue
        fm = frontmatter(skill_md)
        out.append({
            "name": fm.get("name") or entry.name,
            "description": fm.get("description", ""),
            "when_to_use": fm.get("when_to_use", ""),
        })
    return out


def main():
    claude_md = HERE / "USER_CLAUDE.md"
    plan_md = HERE / "hooks" / "PLAN.md"
    template = jinja2.Template((HERE / "rubric.j2").read_text())
    print(template.render(
        path=str(claude_md),
        claude_md=claude_md.read_text().rstrip("\n"),
        plan_md=plan_md.read_text().rstrip("\n"),
        skills=skills(),
    ))


if __name__ == "__main__":
    main()
