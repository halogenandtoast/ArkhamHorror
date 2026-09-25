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
