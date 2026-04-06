---
name: catch-up-on-recent-jj-changes
description: |
  Read recent changes to source files captured by the _Jujutsu_ (_jj_)
  version control tool. Use when reviewing code and discussing the
  user's responses to suggestions.
compatibility: opencode
---

# Scenario

When your user asked you to review source code or other
version-controlled file that he's written, and the _jj_ tool is
involved in recording changes to those files in _commits_, you will
likely offer suggestions and other critique that motivates the user to
change the files in direct response, editing them himself rather than
allowing you to do so on his behalf.

The user will indicate that he's updated the files under discussion,
and wants you to now review what he changed since the last time that
you inspected the files.

The triggering phrases are likely similar to the following:

- "Take a look at my recent updates."
- "I updated the files; please take another look now."
- "Do those updates cover your concerns?"

# Tools Involved

The _jj_ command-line tool offers the _evolog_ subcommand to inspect
its _evolution log_, which records the sequence of updates made to the
same logical _change_. Together with its `--patch` flag, you can
inspect the _diff_ of each update, walking backwards only as far as
the commit that you had inspected previously to prepare your most
recent review feedback.

Invoke the following command to see the diffs of recent revisions to
the same /Jujutsu/ change, presented in reverse-chronological order:

```shell
jj evolog --patch --tool=':git'
```

The `--patch` flag produces the _diff_ output. The `--tool` flag
eliminates any use of an alternate configured _diff_ tool such as
_difftastic_.

The `--revisions` flag allow you to specify from which revisions to
start. The default value is "@", for the current working copy.

You can use the `--limit` flag to specify the count of the revisions
to show, and the `--reversed` flag to reverse the order of the
listing, showing the older revisions before the newer revisions.
