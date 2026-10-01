module AH3e.Content.HeadlineBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { customEffects =
        Map.fromList
          [ ("ignore-rumor", \ctx -> push (IgnoreRumor ctx.investigator))
          , ("rumor-astronomers", eachTestsWill (PlaceDoomAt YourSpace (N 1)))
          , ("rumor-full-moon", eachTestsWill (SufferHorror (N 2)))
          , ("rumor-comet", \ctx -> push (OfferRumorDiscard ctx (Custom "rumor-add-doom")))
          , ("rumor-add-doom", \_ -> push AddRumorDoom)
          , ("why-even-go-on", whyEvenGoOn)
          , ("wild-animal-attacks", wildAnimalAttacks)
          , ("masked-man-mystery", maskedManMystery)
          , ("magician-vanishes", magicianVanishes)
          , ("big-city-burglars-busted", bigCityBurglarsBusted)
          , ("rumor-something-rotten", somethingRotten)
          , ("discard-richest-item", discardRichestItem)
          , ("piscine-pox-onset", piscinePoxOnset)
          , ("rumor-piscine-pox", piscinePoxReckoning)
          ]
    }

-- one question per item, naming it; discarding means that item, not any item
bigCityBurglarsBusted :: EffectCtx -> GameM ()
bigCityBurglarsBusted ctx = do
  items <- matchingAssets ctx.investigator ItemCard
  asks <- for items \cid -> do
    name <- (.name) <$> getCardDef cid
    pure
      $ AskAboutAsset
        ctx.investigator
        cid
        ("Big City Burglars Busted: " <> name)
        [ Choice (CardsLabel ("Discard " <> name) [cid]) [DiscardAsset cid]
        , Choice (TextLabel "Suffer one damage") [SufferHarm ctx.investigator ctx.source NormalHarm 1 0]
        ]
  pushAll asks

eachTestsWill :: Effect -> EffectCtx -> GameM ()
eachTestsWill onFail ctx = do
  invs <- playingInvestigators
  pushAll
    $ [ResolveEffect (ctx & #investigator .~ i.id) (Test Will 0 NoEffect onFail) | i <- invs]
    <> [OfferRumorDiscard ctx NoEffect]

-- rule 474: not a test, so it can't be rerolled
whyEvenGoOn :: EffectCtx -> GameM ()
whyEvenGoOn ctx = do
  n <- rollDie
  logText ("Rolled " <> tshow n)
  chooseFor
    ctx.investigator
    ("Split " <> tshow n <> " between damage and horror")
    [ Choice
        (TextLabel (tshow k <> " damage, " <> tshow (n - k) <> " horror"))
        [SufferHarm ctx.investigator ctx.source NormalHarm k (n - k)]
    | k <- [n, n - 1 .. 0]
    ]

wildAnimalAttacks :: EffectCtx -> GameM ()
wildAnimalAttacks ctx = do
  ms <- uses #monsters Map.elems
  options <- fmap catMaybes $ for ms \m -> do
    d <- monsterDef m.card
    h <- fromMaybe d.health <$> monsterHealth m.card
    pure $ if d.epic then Nothing else Just (m.card, max 0 (h - m.damage))
  chooseFor
    ctx.investigator
    "Choose a non-epic monster"
    [ Choice
        (MonsterLabel mid)
        [SufferHarm ctx.investigator ctx.source NormalHarm remaining 0, DefeatMonster mid ctx.source]
    | (mid, remaining) <- options
    ]

maskedManMystery :: EffectCtx -> GameM ()
maskedManMystery ctx = do
  others <- filter ((/= ctx.investigator) . (.id)) <$> playingInvestigators
  chooseFor
    ctx.investigator
    "Choose another investigator"
    [ Choice (InvestigatorLabel i.id) [SpawnMonsterAt (Just sid) False]
    | i <- others
    , Just sid <- [i.space]
    ]

magicianVanishes :: EffectCtx -> GameM ()
magicianVanishes ctx = do
  let iid = ctx.investigator
  engaged <- map (.card) <$> engagedMonsters iid
  destinations <- unstableSpaces
  let go sid = map (DisengageMonster iid) engaged <> [MoveDirectly iid sid] <> map CheckEngagement engaged
  case destinations of
    [sid] -> pushAll (go sid)
    _ ->
      chooseFor iid "Move to the unstable space" [Choice (SpaceLabel sid) (go sid) | sid <- destinations]

{- | "Reckoning-Spawn one monster. Any investigator may suffer one damage and one
horror to cancel this effect."
-}
somethingRotten :: EffectCtx -> GameM ()
somethingRotten ctx = do
  everyone <- playingInvestigators
  chooseGroup
    "Something Rotten in Arkham: spawn one monster?"
    ( [ Choice
          (InvestigatorLabel i.id)
          [SufferHarm i.id ctx.source NormalHarm 1 1]
      | i <- everyone
      ]
        <> [Choice (DoneLabel "Let it spawn") [ResolveEffect ctx SpawnMonster]]
    )

{- | "Discard the item in the display with the highest value." Ties are the
leader's to break, since nothing on the card says otherwise.
-}
discardRichestItem :: EffectCtx -> GameM ()
discardRichestItem _ = do
  display <- use (#decks . #display)
  valued <- for display \cid -> do
    v <- maybe 0 (fromMaybe 0 . (.value)) <$> assetDef cid
    pure (cid, v)
  case valued of
    [] -> pure ()
    _ -> do
      let best = maximum (map snd valued)
      chooseGroup
        "Discard the item in the display with the highest value"
        [Choice (CardLabel c) [DiscardFromDisplay c, RefillDisplay] | (c, v) <- valued, v == best]

{- | "Each investigator's health is reduced by one." The reduction itself is read
off the rumor by 'investigatorHealth'; this only makes sure anyone already carrying
that much damage is checked against their new, lower health.
-}
piscinePoxOnset :: EffectCtx -> GameM ()
piscinePoxOnset _ = do
  invs <- playingInvestigators
  pushAll [CheckDefeat i.id | i <- invs]

{- | "Reckoning-Any investigator may suffer three damage to discard this card."
A costlier door out than the usual clue, so it is offered to the whole table.
-}
piscinePoxReckoning :: EffectCtx -> GameM ()
piscinePoxReckoning ctx = do
  invs <- playingInvestigators
  chooseGroup "Suffer three damage to discard the rumor?"
    $ [ Choice
          (InvestigatorLabel i.id)
          [SufferHarm i.id ctx.source NormalHarm 3 0, DiscardRumor]
      | i <- invs
      ]
    <> [Choice (DoneLabel "Keep the rumor") []]
