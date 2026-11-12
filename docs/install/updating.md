# Updating an install

For a Docker install or a git checkout. The prebuilt package carries its own
updater — see [Prebuilt package](./prebuilt.md).

Use the upgrade script. It auto-detects your install type (Docker or git
checkout), refreshes control files, pulls the latest images, applies database
migrations, recreates the containers, and re-syncs local images if you have
them.

Run it from anywhere:

```
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/upgrade.sh | bash
```

Or, from inside your install directory (or a git checkout):

```
./upgrade.sh
```

Options (environment variables):

| Variable | Purpose |
| --- | --- |
| `ARKHAM_INSTALL_DIR` | Install directory to upgrade (default: `arkham-horror`). Set this if you installed somewhere else. |
| `ARKHAM_IMAGE_TARGET` | Force an image-sync target (e.g. `en`, `en+fr`, `all`) instead of auto-detecting from what's already on disk. |

```
# Upgrade an install in a custom directory
ARKHAM_INSTALL_DIR=~/games/arkham curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/upgrade.sh | bash

# Force re-syncing English + French images
ARKHAM_IMAGE_TARGET=en+fr ./upgrade.sh
```

The [README](../../README.md#i-just-want-to-try-this-out-on-my-computer)
has the other ways to install it.
