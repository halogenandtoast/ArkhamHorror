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
time, and then you double-click again. Leave the terminal window it opens alone
— closing it or pressing Ctrl+C is how you stop the game, and it writes a
database backup on the way out.

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

[offline/README.md](../../offline/README.md) goes deeper: the codes `start.sh` exits with, which log each
failure lands in, and the order card images are looked up in.

The [README](../../README.md#i-just-want-to-try-this-out-on-my-computer)
has the other ways to install it.
