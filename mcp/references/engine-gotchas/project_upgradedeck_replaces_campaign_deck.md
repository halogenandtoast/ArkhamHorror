---
name: project_upgradedeck_replaces_campaign_deck
description: UpgradeDeck overwrites campaignDecks wholesale from the submitted decklist, so engine-side removals of ordinary player cards don't survive the next upgrade step
metadata:
  type: project
---

`Campaign/Runner.hs`'s `UpgradeDeck iid mUrl deck` ends with `updateAttrs a $ decksL %~ insertMap iid deck'` — the submitted decklist **replaces** the stored campaign deck rather than being merged into it.

Consequence: `RemoveCampaignCardFromDeck` is only durable for cards the deckbuilder will never re-add — story/weakness cards the player does not control (Hospital Debts, Jim's Trumpet, a Task asset). Removing an *ordinary purchasable player card* engine-side is clobbered at the next `UpgradeDeckStep`.

That is why card text of the form "remove cards from your deck worth N experience / you cannot purchase those cards again" is routed to the deckbuilder with a note in the locale rather than implemented as a choice. The Drowned City's "Prove Your Worth" failure does this:

```
failedEffect: "… Remove cards of level 1–5 from your deck worth a total of 10 or more
experience … <span style='color:purple'>*Please remove the cards manually in your deck builder.*</span>"
```

Also note `RemoveCampaignCardFromDeck` filters by `CardDef`, so it removes *every* copy of that def — it cannot express "remove one of your two copies."

Related: [[project_forcedchaostokenchange_token_target]]
