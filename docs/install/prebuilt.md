# Prebuilt package (no Docker)

The least work, if there is a build for your machine. One archive with the
backend, the frontend, PostgreSQL and nginx already in it — nothing to
install, not even Docker.

Grab the archive for your platform from the
[releases page](https://github.com/halogenandtoast/ArkhamHorror/releases):

| You have | Download | Then |
| --- | --- | --- |
| An Apple Silicon Mac | `ArkhamHorror-macos-arm64.tar.gz` | double-click `Start-ArkhamHorror.command` |
| Windows | `ArkhamHorror-linux-x86_64.tar.gz` | double-click `Start-ArkhamHorror.bat` |
| Linux on x86_64 | `ArkhamHorror-linux-x86_64.tar.gz` | `bash game/start.sh` |

Yes, Windows takes the Linux archive: the launcher runs it inside WSL. It
installs WSL and Ubuntu if they are missing, which wants a reboot the first
time, and then you double-click again — that first launch needs
**Run as administrator**, because installing WSL asks for elevation. Leave the
terminal window it opens alone — closing it or pressing Ctrl+C is how you stop
the game, and it writes a database backup on the way out.

If extracting the archive on Windows fails on a symbolic link, turn on
**Developer Mode** (Settings → System → For developers) and extract again;
Windows will not create symlinks without it. The launcher repairs the library
links itself afterwards, so a copy in place of a link is fine — a failed
extraction is not.

[The Windows section of offline/README.md](../../offline/README.md#windows) has
the whole first-run sequence, step by step, if a launch stops partway.

The first start initialises its own database, imports `setup.sql`, brings up all
three services and opens http://localhost:3000. Later starts reuse the data.
`bash game/start.sh --status` and `--stop` do what they say.

Your saved games are kept outside the package, so a newer one does not disturb
them:

* macOS — `~/Library/Application Support/ArkhamHorror/`
* Linux and WSL — `~/.local/share/ArkhamHorror/`

A normal shutdown also writes a dump to the package's own `backup/`, which is
what a later install restores from if it finds no live database.

Images come from the CDN, and nginx falls back to it file by file, so you can
drop as much or as little local art as you like into `game/frontend/dist/img/`.
Your own card images go in `cards/` (or `cards_en/`), which are checked first.

To upgrade, download the new archive into the same folder as the old one and
double-click `Update-ArkhamHorror.command` (`.bat` on Windows). It stops the
services, swaps `game/` for the new one and leaves `cards/`, `cards_en/` and
`backup/` where they are. It will not download anything itself.

## When something goes wrong

Everything the launcher knows is in
[offline/README.md](../../offline/README.md):

* [Windows, start to finish](../../offline/README.md#windows) — what
  `start.bat` does at each step, and which error code belongs to which step.
* [Error codes](../../offline/README.md#error-code-quick-reference) — every
  code `start.sh` exits with and the log file that explains it.
* [FAQ](../../offline/README.md#faq) — card images and the CDN fallback, WSL
  and `sudo`, `initdb` permissions, why the cmd window stays open.

The logs themselves are in the package's own `data/` directory —
`pg.log`, `arkham-api.log`, `error.log` and the rest. They are kept when a
start fails and cleared after a clean shutdown.

The [README](../../README.md#i-just-want-to-try-this-out-on-my-computer)
has the other ways to install it.
