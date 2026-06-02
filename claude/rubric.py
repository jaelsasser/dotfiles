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
    def read_file(path):
        return (HERE / path).read_text().rstrip("\n")

    env = jinja2.Environment()
    env.globals["read_file"] = read_file
    env.globals["list_skills"] = skills

    template = env.from_string((HERE / "rubric.j2").read_text())
    print(template.render())


if __name__ == "__main__":
    main()
