---
name: pr-stack
description: Change, merge or reorder pull requests that are stacked on each other (each PR's base is the previous PR's branch) - fold a fix into the PR whose code it touches, rebuild the PRs above it, verify every layer, and force-push and retarget them. Use when a review comment, a simplification or a bug fix belongs in a lower PR of a stack, when PRs in a stack should be merged or dropped, or when asked to rebase a stack.
---

# Working with a stack of PRs

find-unused's features often land as a stack: each PR's branch is based on the one below it, and only the bottom one
targets `main`. List it with `gh pr list --author @me --state open --json number,headRefName,baseRefName,title`.
A change that belongs in a lower PR has to be folded into it, and every PR above rebuilt on top, so each PR's diff
keeps showing the code as it will be merged rather than code a later PR rewrites.

## 1. Decide where each change goes

- **Map changed lines to PRs.** Write the stack's branches bottom up, one per line, starting with `main`, to a file, and
  run `blame_layers.py <layers file> <top branch> <branch with the change>`. For each hunk it shows which PRs last
  touched the lines it replaces.
- **Fold a change into the PR whose code it changes,** or where the duplication it removes first appears. For example,
  a helper that replaces two copies belongs in the PR that adds the second copy. A change that's a consequence of one
  PR's design goes there; e.g. removing a merge that a performance PR made redundant goes into that PR.
- **Prefer the topmost PR that fits.** Every PR above a fold has to be rebuilt, so this keeps conflicts down.
- **A behavior change gets its own PR,** usually on top, with its scripted test. Don't hide it in a refactoring.
- **Merge PRs when that makes review easier:** adjacent, small, related ones. Moving a PR down past others is the
  same work as folding, and it may depend on something in between (check what its code uses).

## 2. Rebuild bottom up

1. **Back up first.** Tag every branch head (`git tag orig/<branch> <branch>`). Do the work on new local branches,
   e.g. `rs/<branch>`, and move the real branches only at the end.
2. **Keep untouched PRs as they are.** PRs below the first fold keep their branches and commits. A PR whose base is
   unchanged can keep its commits plus one commit for its fold.
3. **Rebuild every PR above as one commit.** For each, run
   `apply_layer.sh orig/<branch below> orig/<branch> rs/<branch below> rs/<branch>`, which 3-way applies the PR's
   whole diff onto the new base and lists conflicts.
   - Resolve them with an editor or `resolve_conflicts.py <file> <resolutions.json>`.
   - Make the PR's own new code match the style the folds introduced.
   - Make the docs (CLAUDE.md, README, skills) at that layer describe that layer's code.
   - Run `sbt test`, which compiles everything with `-Werror`, so an import a fold made unused fails here.
   - Commit with the PR's title and a one-paragraph body.
4. **Use the final tree as the reference.** The top PR's tree should be exactly the tree that was verified as a whole
   (`git read-tree -u --reset <verified commit>`, then check `git diff`). For a lower PR, any file that no PR above it
   touches can be taken from the verified tree, as long as no fold above it changes that file too.
5. **Watch for leftovers.** Folds leave stale comments next to code they replaced. Read each rebuilt layer's diff
   against its original (`git diff orig/<branch> rs/<branch>`): everything in it should be a fold.

## 3. Verify every rebuilt PR

- **Scripted:** run `sbt 'test; scripted'` for each rebuilt branch, one branch at a time, in a scratch clone
  (`git clone <repo> <scratch>/stack-test`, then check out each `rs/` branch). Scripted publishes the plugin locally
  under the snapshot version, so two runs at once overwrite each other. See the `scripted-test` skill.
- **Output:** for PRs whose analysis changed, compare each rebuilt branch's CLI with its original's on a real project
  with `compare_builds.sh` (`validate-on-project` skill). Refactoring folds must give identical output.
- **Top of the stack:** check it on every Scala version CI uses (`-Dscripted.scala.version=<v>`).

## 4. Publish

1. Push each branch: `git push --force-with-lease origin rs/<branch>:<branch>`, then
   `git branch -f <branch> rs/<branch>` locally.
2. For a PR that was merged into another or dropped:
   - retarget the PR above it first, `gh pr edit <n> --base <new base>`;
   - then close it with `gh pr close <n> --comment "..."`, saying where its content went;
   - then delete its branch.
3. Push a new PR's branch and open it with `gh pr create --draft --base <branch below>`, unless asked otherwise.
4. Update each changed PR's description, `gh pr edit <n> --body-file <file>`, including its "Depends on" line when its
   base changed. Mark PRs ready with `gh pr ready <n>` only when asked.
