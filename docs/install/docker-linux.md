# Installing with Docker on Linux

Install [Docker](https://www.docker.com/), then run:

```
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/install.sh | bash
```

This creates an `arkham-horror/` directory, downloads the required files, generates
a database password, and starts the app. Open http://localhost:3000 when it's done.

The script will ask if you want to download game images (~2.9 GB). If you skip
this step, the app loads images from the CDN automatically — no extra setup needed.

The [README](../../README.md#i-just-want-to-try-this-out-on-my-computer)
has the other ways to install it.
