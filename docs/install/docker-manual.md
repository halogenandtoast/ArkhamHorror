# Installing with Docker by hand

If you prefer not to use the install script, you'll need four files:

```
mkdir -p arkham-horror/config arkham-horror/scripts arkham-horror/frontend/public/img
cd arkham-horror
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/docker-compose.yml -o docker-compose.yml
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/setup.sql -o setup.sql
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/scripts/fetch-assets.sh -o scripts/fetch-assets.sh
# Generate a strong password
openssl rand -base64 32 > config/postgres_password.txt
docker compose up -d
```

The app automatically detects whether local images are present:
- **No local images** (default after install) → images load from the CDN automatically
- **Local images present** (after running the fetch service) → images served locally

To fetch images locally, use the `fetch-images` Docker service.
Once fetched, restart with `docker compose restart web` and the app will switch to local images.

```
# English only (~1.3 GB) — portraits, tokens, icons, card images
docker compose --profile fetch-images run --rm fetch-images en

# English + a specific language (recommended for non-English play)
docker compose --profile fetch-images run --rm fetch-images en+fr   # French
docker compose --profile fetch-images run --rm fetch-images en+es   # Spanish
docker compose --profile fetch-images run --rm fetch-images en+ita  # Italian
docker compose --profile fetch-images run --rm fetch-images en+ko   # Korean
docker compose --profile fetch-images run --rm fetch-images en+zh   # Chinese

# A single language's card translations only (English assets still load from CDN)
docker compose --profile fetch-images run --rm fetch-images fr

# Everything (~2.9 GB)
docker compose --profile fetch-images run --rm fetch-images all
```

Images are stored in `frontend/public/img/` and mounted into the container.
After fetching, run `docker compose restart web` to pick them up.

To switch back to CDN at any time, add this to the `web` service environment in `docker-compose.yml`:

```yaml
    environment:
      - ASSET_HOST=https://assets.arkhamhorror.app
```

The [README](../../README.md#i-just-want-to-try-this-out-on-my-computer)
has the other ways to install it.
