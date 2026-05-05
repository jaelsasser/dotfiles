#!/usr/bin/env python3
"""Emit a paste-ready prompt asking a cold Claude session to grade a CLAUDE.md."""

import argparse
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
SKILLS_DIR = SCRIPT_DIR / "skills"


def parse_frontmatter(text):
    lines = text.splitlines()
    if not lines or lines[0].strip() != "---":
        return {}
    out = {}
    for line in lines[1:]:
        if line.strip() == "---":
            break
        if ":" in line:
            key, _, value = line.partition(":")
            out[key.strip()] = value.strip()
    return out


def load_skills():
    if not SKILLS_DIR.is_dir():
        return []
    skills = []
    for entry in sorted(SKILLS_DIR.iterdir()):
        if not entry.is_dir():
            continue
        skill_md = entry / "SKILL.md"
        if not skill_md.is_file():
            continue
        text = skill_md.read_text()
        fm = parse_frontmatter(text)
        name = fm.get("name") or entry.name
        skills.append((name, fm, text))
    return skills


def format_skill_listing(skills):
    lines = [
        "The following user-provided skills are dotfiled and always available for use with the Skill"
        "tool, presented as they would appear injected into the operator's context:",
        "",
    ]
    for name, fm, _ in skills:
        desc = fm.get("description", "")
        when = fm.get("when_to_use", "")
        if when:
            lines.append(f" - {name}: {desc} - {when}")
        else:
            lines.append(f" - {name}: {desc}")
    return "\n".join(lines)


def cmd_grade(path):
    contents = path.read_text().rstrip("\n")
    skills = load_skills()
    parts = [
        "Grade this CLAUDE.md from the perspective of an LLM who's going to get this"
        "fed into their <system_reminder> in the next session (because it will be)."
        "Three notes: (a) this layers **on top** of your Claude Code harness — negative space"
        "is often intentional fallthrough; (b) this layers **beneath** a repo-level"
        "CLAUDE.md, and (c) tone guidance is a userStyle concern and not graded here."
        "",
        "I offload facts about where we to ~/.claude/rules, all cold-start operators"
        "know if they're "
        "",
        "<system_reminder_to_grade>",
        "",
        "#claudeMd",
        "",
        f" Contents of {path} (user's private global instructions for all projects, dotfiled):",
        contents,
        "",
        "</system_reminder_to_grade>",
    ]
    if skills:
        parts += ["", format_skill_listing(skills)]
        parts += [
            "",
            f"To see a skill's full body, run `{Path(__file__).name} --skill <name>`"
        ]
    return "\n".join(parts)


def cmd_skill(name):
    for skill_name, _, text in load_skills():
        if skill_name == name:
            body = text.rstrip("\n")
            return f"<skill_to_grade>\n{body}\n</skill_to_grade>"
    sys.exit(f"error: skill {name!r} not found under {SKILLS_DIR}")


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("path", nargs="?", type=Path, help="CLAUDE.md to grade")
    p.add_argument("--skill", metavar="NAME", help="emit a single skill's full body")
    args = p.parse_args()

    if args.skill and args.path:
        p.error("pass a path OR --skill, not both")
    if args.skill:
        print(cmd_skill(args.skill))
    elif args.path:
        print(cmd_grade(args.path))
    else:
        print(cmd_grade(SCRIPT_DIR / "USER_CLAUDE.md"))


if __name__ == "__main__":
    main()
