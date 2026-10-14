---
name: project_watcher_server_runs_the_previous_builds_binary
description: The api.watch server is launched before the current build finishes installing, so a change can take two watcher cycles to reach the running server and looks like it did nothing
metadata:
  type: project
---

`make api.watch*` builds, then `--exec` does `pkill arkham-api; stack exec
arkham-api & stack build --test ...`. `stack exec` runs the **installed** copy
under `.stack-work/install/.../bin/arkham-api`, and the install for the build
that just ran can land *after* the server is relaunched. The server then runs
the **previous** build's binary, and an engine change looks like it simply did
not work.

Check it directly, not by reading the log:

```
ps -o lstart= -p $(pgrep -n arkham-api)
find backend/.stack-work/install -path '*9.14*' -name arkham-api \
  -exec stat -f '%Sm' -t '%H:%M:%S' {} \;
```

If the binary is newer than the process, the change is not live. `touch` any
module to force another cycle; the next restart picks it up.

This cost an hour chasing a `Discard` arm that was correct all along, and it is
the same trap as [[project_stack_exec_arkham_replay_can_be_a_stale_copy]] --
`stack exec` resolves the install, never the just-linked dist binary.
