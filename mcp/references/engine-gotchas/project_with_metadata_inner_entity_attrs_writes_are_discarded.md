# A `With`-metadata inner entity's attrs writes are thrown away

`instance Entity (a `With` b)` reads `toAttrs` from the **outer** half only
(`Classes/Entity.hs:35`). So a card that parks a whole second entity in its
metadata and routes messages to it —

```haskell
_ -> case currentAsset meta of
  Just iasset -> do
    iasset' <- lift $ runMessage msg iasset
    pure $ TrueMagickReworkingReality5 $ With attrs (Metadata $ Just iasset')
```

— persists the inner entity but never the attrs it just mutated. Every field
query, every JSON export and every subsequent cost read goes to `attrs`, and the
inner copy dies when the metadata resets.

True Magick (Reworking Reality) (5) builds the borrowed in-hand spell as its own
attrs under the spell's card code, so the borrowed ability correctly targets
`AssetTarget trueMagickId` — the trace shows `TokenMessage (RemoveTokens_ … Charge 1)`
being processed — yet the charge stayed on the card: the removal landed in the
metadata copy and was discarded at `ResolvedAbility` (#5801). Fixed by pushing the
live attrs in and pulling them back out around every routed message.

Note the asymmetry this produced: a charge paid as an ability **cost**
(`UseCost`) works, because `ActiveCost` settles before the `UseCardAbility`
handler installs the metadata. Only post-activation spends were lost. And the
metadata is `null` at every step boundary in an export, so this is invisible to
`arkham-replay --replay-all` and to any patch-level diff — it only shows up
mid-step.
