.PHONY: api.watch
api.watch:
	cd arkham-api && \
	stack build --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
	  --exec "bash -c \"pkill arkham-api; DEVELOPMENT=true stack exec arkham-api &\"" \

## Generate a tags file for source-code navigation
.PHONY: tags
tags:
	stack exec -- fast-tags --qualified -Rv --nomerge .

data:
	cd data && \
	curl https://arkhamdb.com/api/public/cards > cards.json && \
	curl https://arkhamdb.com/api/public/packs/ | jq ".[].code" | xargs sh -c 'for arg do mkdir $arg; curl https://arkhamdb.com/api/public/cards/$arg > $arg/cards.json; done' _
