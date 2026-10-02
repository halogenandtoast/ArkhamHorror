{- | Condition mechanics. Blessed and cursed are handled by the engine (they
change the success threshold and are spent by a test); everything here hangs
off a card's own reckoning.
-}
module AH3e.Content.ConditionBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

darkPacts :: [CardCode]
darkPacts =
  [ "dark-pact-virulent-plague"
  , "dark-pact-grim-spectre"
  , "dark-pact-the-world-undone"
  , "dark-pact-pact-of-sacrifice"
  , "dark-pact-the-ultimate-price"
  , "dark-pact-an-alliance-of-evil"
  , "dark-pact-dark-destiny"
  , "dark-pact-forbidden-knowledge"
  ]

{- | Dead of Night's WANTED cards. One front, and a different reckoning on each
back for whatever finally catches up with you.
-}
wantedCards :: [CardCode]
wantedCards =
  [ "wanted-beaten"
  , "wanted-detained"
  , "wanted-disarmed"
  , "wanted-rattled"
  , "wanted-shaken-down"
  , "wanted-vengeful-pursuer"
  ]

{- | Under Dark Waves' TAINTED cards. One front, which bleeds doom into your space
whenever the mythos turns up nothing, and six numbered backs for what it finally
does to you.
-}
taintedCards :: [CardCode]
taintedCards = [CardCode ("tainted-" <> tshow n) | n <- [1 :: Int .. 6]]

behaviors :: Behaviors
behaviors =
  mempty
    { assets =
        Map.fromList
          $ [(c, darkPactBehavior) | c <- darkPacts]
          <> [(c, wantedBehavior c) | c <- wantedCards]
          <> [(c, taintedBehavior) | c <- taintedCards]
          <> [("driven", drivenBehavior)]
    , customEffects =
        Map.fromList
          [ ("driven-extra-action", drivenExtraAction)
          , ("dark-pact-reckoning", darkPactReckoning)
          , ("virulent-plague", virulentPlague)
          , ("grim-spectre", grimSpectre)
          , ("tainted-reckoning", taintedReckoning)
          , ("tainted-lash-out:2", taintedLashOut 2)
          , ("tainted-lash-out:1", taintedLashOut 1)
          , ("tainted-summon", taintedSummon)
          , ("tainted-summoned-attacks", taintedSummonedAttacks)
          , ("tainted-forget", taintedForget)
          , ("tainted-discard-focus", taintedDiscardFocus)
          , ("flip-condition", flipCondition)
          , ("discard-condition", discardCondition)
          , ("pact-of-sacrifice", pactOfSacrifice)
          , ("an-alliance-of-evil", allianceOfEvil)
          , ("an-alliance-of-evil-attacks", allianceOfEvilAttacks)
          , ("forbidden-knowledge", forbiddenKnowledge 3)
          , ("forbidden-knowledge:2", forbiddenKnowledge 2)
          , ("forbidden-knowledge:1", forbiddenKnowledge 1)
          , ("forbidden-knowledge:0", forbiddenKnowledge 0)
          , ("wanted-reckoning", wantedReckoning)
          , ("wanted-disarm", disarm)
          , ("wanted-shake-down", shakeDown)
          , ("wanted-vengeful-pursuer", vengefulPursuer)
          ]
    , customActivations =
        Map.fromList
          [ ("vengeful-pursuer", pursuerActivation)
          , ("grim-spectre", spectreActivation)
          ]
    }

darkPactBehavior :: AssetBehavior
darkPactBehavior = defaultAssetBehavior & #reckoning ?~ Custom "dark-pact-reckoning"

{- | DRIVEN, with the FATIGUED it becomes on its back. "Your focus limit is
increased by one" is read off 'AH3e.Content.Conditions.focusLimitBonuses', which
only counts the side showing. The rest is what each side does: the drive turns
itself over for an extra action, and the fatigue it leaves charges a die for
every reroll until a focus action clears it.
-}
drivenBehavior :: AssetBehavior
drivenBehavior =
  defaultAssetBehavior
    & #rerollRemovesADie
    .~ showingFatigue
    & #afterOwnerAction
    .~ ( \cid iid kind -> do
           tired <- showingFatigue cid iid
           pure [DiscardAsset cid | tired, kind == FocusAction]
       )
    & #reactions
    .~ \cid -> \case
      AtEndOfTurn iid -> do
        a <- use (assetL cid)
        -- holding the drive and the fatigue at once takes two copies of the card
        tired <- hasCondition iid "FATIGUED"
        pure
          [ Reaction
              "driven"
              "Driven: turn the card over to perform one additional action"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Custom "driven-extra-action")]
          | a.owner == iid
          , not a.flipped
          , not tired
          ]
      _ -> pure []

-- | Whether this card is theirs and turned to its FATIGUED side.
showingFatigue :: CardId -> InvestigatorId -> GameM Bool
showingFatigue cid iid = do
  a <- use (assetL cid)
  pure (a.owner == iid && a.flipped)

{- | "You may flip this card to perform one additional action." The action is
granted first, so the turn it is spent in is still running when the card turns
over; the turn is then handed back rather than ending.
-}
drivenExtraAction :: EffectCtx -> GameM ()
drivenExtraAction ctx = do
  investigatorL ctx.investigator . #bonusActions += 1
  logText "Driven: one more action, and the fatigue to come"
  pushAll [ResolveEffect ctx (Custom "flip-condition"), ActionTurn ctx.investigator]

discardSelf :: Effect
discardSelf = Custom "discard-condition"

-- | What the exhausted side of a dark pact does the moment it is revealed.
revealedSide :: CardCode -> Maybe Effect
revealedSide = \case
  "dark-pact-virulent-plague" -> Just (Custom "virulent-plague")
  "dark-pact-grim-spectre" -> Just (Custom "grim-spectre")
  "tainted-1" ->
    taintedBack
      (Custom "tainted-lash-out:2")
      [((1, Just 1), Custom "tainted-lash-out:1"), ((2, Nothing), NoEffect)]
  "tainted-2" ->
    taintedBack
      (GainE (Condition "CURSED"))
      [((1, Just 1), GainE (Condition "CURSED")), ((2, Nothing), NoEffect)]
  "tainted-3" ->
    taintedBack
      (SufferHarmE (N 2) (N 2))
      [((1, Just 1), SufferHarmE (N 1) (N 1)), ((2, Nothing), NoEffect)]
  "tainted-4" ->
    taintedBack
      (GainE (Condition "DARK PACT"))
      [((1, Just 1), GainE (Condition "DARK PACT")), ((2, Nothing), NoEffect)]
  "tainted-5" ->
    taintedBack
      (Custom "tainted-summon")
      [((1, Just 1), SpawnMonsterIn YourSpace False), ((2, Nothing), NoEffect)]
  "tainted-6" ->
    taintedBack
      (Custom "tainted-forget")
      [((1, Just 2), Custom "tainted-discard-focus"), ((3, Nothing), NoEffect)]
  "dark-pact-the-world-undone" -> Just (Seq [PlaceDoomAt YourSpace (N 3), discardSelf])
  "dark-pact-the-ultimate-price" -> Just BecomeDevoured
  "dark-pact-dark-destiny" -> Just (Seq [DrawMythosTokens 6, discardSelf])
  "dark-pact-pact-of-sacrifice" -> Just (Custom "pact-of-sacrifice")
  "dark-pact-an-alliance-of-evil" -> Just (Custom "an-alliance-of-evil")
  "dark-pact-forbidden-knowledge" -> Just (Custom "forbidden-knowledge")
  _ -> Nothing

-- | The reckoning of a revealed side, for a card that keeps playing once flipped.
revealedReckoning :: CardCode -> Maybe Effect
revealedReckoning = \case
  -- RATTLED stays in play for one die, and clears itself at the next reckoning
  "wanted-rattled" -> Just discardSelf
  _ -> Nothing

{- | A WANTED card. Face up it is the law closing in -- an influence test each
reckoning, passed well enough to shake them off or failed into whatever the back
has waiting. Its reckoning fires from whichever side is showing.
-}

{- | The traits WANTED keeps you out of: every faction's reputation talent. A new
faction's reputation needs adding here as well as to its own cards.
-}
reputationTraits :: [Trait]
reputationTraits =
  ["Arkham Reputation", "O'Bannion Reputation", "Police Reputation", "Sheldon Reputation"]

wantedBehavior :: CardCode -> AssetBehavior
wantedBehavior code =
  defaultAssetBehavior
    & #reckoning
    ?~ Custom "wanted-reckoning"
    & #bansTraits
    .~ reputationTraits
    & #poolDelta
    .~ \cid iid _ ->
      if code /= "wanted-rattled"
        then pure 0
        else do
          flipped <- maybe False (.flipped) <$> uses #assets (Map.lookup cid)
          owned <- uses #assets (maybe False ((== iid) . (.owner)) . Map.lookup cid)
          pure (if flipped && owned then -1 else 0)

{- | Front: test influence, shake them off on a two or better, and flip on a
failure. Back: whatever they did to you when they caught up.
-}
wantedReckoning :: EffectCtx -> GameM ()
wantedReckoning ctx = for_ (sourceCard ctx) \cid -> do
  flipped <- maybe False (.flipped) <$> uses #assets (Map.lookup cid)
  code <- cardCode cid
  if flipped
    then for_ (caughtUp code) \eff -> push (ResolveEffect ctx eff)
    else
      push
        ( ResolveEffect
            ctx
            (Test Influence 0 (ByResult [((2, Nothing), discardSelf)]) (Custom "flip-condition"))
        )

-- | The back of each WANTED card, read the moment the reckoning turns it over.
caughtUp :: CardCode -> Maybe Effect
caughtUp = \case
  "wanted-beaten" -> Just (Seq [SufferDamage (N 2), discardSelf])
  "wanted-detained" -> Just (Seq [BecomeDelayed, discardSelf])
  "wanted-disarmed" -> Just (Custom "wanted-disarm")
  "wanted-shaken-down" -> Just (Custom "wanted-shake-down")
  "wanted-vengeful-pursuer" -> Just (Custom "wanted-vengeful-pursuer")
  -- RATTLED is the one that lingers; its own reckoning discards it
  _ -> Nothing

-- | "You discard one non-curio weapon."
disarm :: EffectCtx -> GameM ()
disarm ctx = do
  i <- getInvestigator ctx.investigator
  weapons <- filterM (fmap (maybe False isWeapon) . assetDef) i.assets
  case weapons of
    [] -> push (ResolveEffect ctx discardSelf)
    _ ->
      chooseFor
        ctx.investigator
        "Discard a weapon"
        [Choice (CardLabel c) [DiscardAsset c, ResolveEffect ctx discardSelf] | c <- weapons]
 where
  isWeapon :: AssetDef -> Bool
  isWeapon d = "Weapon" `elem` d.traits && "Curio" `notElem` d.traits

-- | "You discard all of your money."
shakeDown :: EffectCtx -> GameM ()
shakeDown ctx = do
  i <- getInvestigator ctx.investigator
  addMoney ctx.investigator (negate i.money)
  logText "They take every cent you have"
  push (ResolveEffect ctx discardSelf)

{- | The back that is a monster: it comes for you where you stand, and wanders off
again the moment nobody is holding it.
-}
vengefulPursuer :: EffectCtx -> GameM ()
vengefulPursuer ctx = do
  msid <- investigatorSpace ctx.investigator
  case msid of
    Nothing -> push (ResolveEffect ctx discardSelf)
    Just sid -> do
      cid <- newCard "vengeful-pursuer"
      pushAll
        [ PlaceMonster cid sid Ready
        , EngageMonster ctx.investigator cid
        , ResolveEffect ctx discardSelf
        ]

sourceCard :: EffectCtx -> Maybe CardId
sourceCard ctx = case ctx.source of
  SourceCard cid -> Just cid
  _ -> Nothing

-- rule 474: a roll outside a test, so nothing can reroll or modify it
darkPactReckoning :: EffectCtx -> GameM ()
darkPactReckoning ctx = for_ (sourceCard ctx) \cid -> do
  a <- use (assetL cid)
  code <- cardCode cid
  if a.flipped
    then for_ (revealedReckoning code) (push . ResolveEffect ctx)
    else do
      i <- getInvestigator ctx.investigator
      codes <- traverse cardCode i.assets
      let dice = if "dark-blessing" `elem` codes then 2 else 1
      vs <- replicateM dice rollDie
      logText ("Dark pact: rolled " <> tshow vs)
      when (1 `elem` vs) do
        logText "Your debt has come due"
        push (ResolveEffect ctx (Custom "flip-condition"))

flipCondition :: EffectCtx -> GameM ()
flipCondition ctx = for_ (sourceCard ctx) \cid -> do
  assetL cid . #flipped %= not
  a <- use (assetL cid)
  code <- cardCode cid
  when a.flipped $ for_ (revealedSide code) (push . ResolveEffect ctx)

discardCondition :: EffectCtx -> GameM ()
discardCondition ctx = for_ (sourceCard ctx) (push . DiscardAsset)

-- "Choose another investigator on any space. That investigator is devoured."
pactOfSacrifice :: EffectCtx -> GameM ()
pactOfSacrifice ctx = do
  others <- filter ((/= ctx.investigator) . (.id)) <$> playingInvestigators
  case others of
    [] -> logText "No other investigator to sacrifice"
    _ ->
      chooseFor ctx.investigator "Choose an investigator to be devoured"
        $ [ Choice (InvestigatorLabel o.id) [DevourInvestigator o.id, DiscardAsset cid]
          | o <- others
          , cid <- toList (sourceCard ctx)
          ]

{- | "Spawn one monster in each space in your neighborhood. Each monster
recovers all of its health and deals damage and horror to the investigator it
has engaged." The heal only means anything for monsters already on the board,
so the second sentence covers every monster, not just the new ones.
-}
allianceOfEvil :: EffectCtx -> GameM ()
allianceOfEvil ctx = do
  mnid <- investigatorNeighborhood ctx.investigator
  board <- use #board
  let spaces = maybe [] (`neighborhoodSpaces` board) mnid
  pushAll
    ( [SpawnMonsterAt (Just sid) False | sid <- spaces]
        <> [ResolveEffect ctx (Custom "an-alliance-of-evil-attacks")]
    )

allianceOfEvilAttacks :: EffectCtx -> GameM ()
allianceOfEvilAttacks ctx = do
  #monsters . traverse . #damage .= 0
  logText "Every monster recovers all of its health"
  ms <- uses #monsters Map.elems
  let engaged m = case m.state of
        Engaged is -> is
        _ -> []
  pushAll
    ( [MonsterAttacks m.card i | m <- ms, i <- engaged m]
        <> [DiscardAsset cid | cid <- toList (sourceCard ctx)]
    )

{- | "Discard three clues total from among all investigators and the scenario
sheet (or all such clues if there are fewer than three). If exactly zero or one
clue is discarded this way, place one doom on the scenario sheet." The remaining
count is carried in the effect key, so the number discarded is 3 - remaining.
-}
forbiddenKnowledge :: Int -> EffectCtx -> GameM ()
forbiddenKnowledge remaining ctx = do
  invs <- playingInvestigators
  sheet <- use #sheetClues
  let sources =
        [Choice (InvestigatorLabel i.id) [DiscardClue (Just i.id)] | i <- invs, i.clues > 0]
          <> [Choice (TextLabel "Scenario sheet") [DiscardClue Nothing] | sheet > 0]
      next = Custom ("forbidden-knowledge:" <> tshow (remaining - 1))
  if remaining <= 0 || null sources
    then do
      let discarded = 3 - remaining
      when (discarded <= 1) do
        logText "Too little knowledge is given up; one doom goes on the scenario sheet"
        push (PlaceDoomOnSheet 1)
      for_ (sourceCard ctx) (push . DiscardAsset)
    else
      chooseFor
        ctx.investigator
        "Discard a clue"
        [Choice l (ms <> [ResolveEffect ctx next]) | Choice l ms <- sources]

{- | TAINTED's front: doom collects wherever you stand each time the mythos turns
up nothing, and the first reckoning turns the card over.
-}
taintedBehavior :: AssetBehavior
taintedBehavior =
  defaultAssetBehavior
    & #reckoning
    ?~ Custom "tainted-reckoning"
    & #afterMythosToken
    .~ \cid iid tok -> do
      flipped <- maybe False (.flipped) <$> uses #assets (Map.lookup cid)
      pure
        [ ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (PlaceDoomAt YourSpace (N 1))
        | not flipped
        , tok `elem` [BlankToken, SpawnClueToken]
        ]

-- | "Reckoning-Flip this card." The back resolves as it is turned over.
taintedReckoning :: EffectCtx -> GameM ()
taintedReckoning ctx = for_ (sourceCard ctx) \cid -> do
  flipped <- maybe False (.flipped) <$> uses #assets (Map.lookup cid)
  unless flipped $ push (ResolveEffect ctx (Custom "flip-condition"))

{- | Every TAINTED back reads the same way: test will, resolve the band your
result falls in, then discard the card. A zero is a failed test, so it rides on
the test's fail branch while the rest are read off the result.
-}
taintedBack :: Effect -> [((Int, Maybe Int), Effect)] -> Maybe Effect
taintedBack onZero bands = Just (Seq [Test Will 0 (ByResult bands) onZero, discardSelf])

{- | "Choose another investigator in any space to suffer this much damage and
horror." Not a may, so the choice is only which of them it falls on.
-}
taintedLashOut :: Int -> EffectCtx -> GameM ()
taintedLashOut n ctx = do
  others <- filter ((/= ctx.investigator) . (.id)) <$> playingInvestigators
  case others of
    [] -> logText "There is nobody else to turn on"
    _ ->
      chooseFor
        ctx.investigator
        ("Choose an investigator to suffer " <> tshow n <> " damage and " <> tshow n <> " horror")
        $ [Choice (InvestigatorLabel o.id) [SufferHarm o.id ctx.source NormalHarm n n] | o <- others]

{- | "Spawn one monster in your space. If it engages an investigator, that monster
attacks." The monster is drawn here rather than through 'SpawnMonsterAt' so that
its card is known afterwards; it is noted on the condition until the attack check.
-}
taintedSummon :: EffectCtx -> GameM ()
taintedSummon ctx = for_ (sourceCard ctx) \self -> do
  msid <- investigatorSpace ctx.investigator
  deck <- use (#decks . #monster)
  case (msid, drawBottom deck) of
    (Just sid, Just (mid, rest)) -> do
      #decks . #monster .= rest
      pushAll
        [ PlaceMonster mid sid Ready
        , NoteOnCard self "summoned" (coerce mid)
        , ResolveEffect ctx (Custom "tainted-summoned-attacks")
        ]
    _ -> logText "No monster answers"

-- | The monster the card just summoned attacks whoever it caught.
taintedSummonedAttacks :: EffectCtx -> GameM ()
taintedSummonedAttacks ctx = for_ (sourceCard ctx) \self -> do
  noted <- uses #assets (Map.lookup "summoned" . maybe mempty (.tokens) . Map.lookup self)
  for_ noted \n -> do
    assetL self . #tokens .= mempty
    m <- uses #monsters (Map.lookup (CardId n))
    for_ m \mon -> case mon.state of
      Engaged (iid : _) -> push (MonsterAttacks (CardId n) iid)
      _ -> pure ()

{- | "You discard one talent. If you cannot, you discard all of your focus
tokens."
-}
taintedForget :: EffectCtx -> GameM ()
taintedForget ctx = do
  i <- getInvestigator ctx.investigator
  talents <- filterM isTalent i.assets
  case talents of
    [] -> discardAllFocus ctx.investigator
    _ ->
      chooseFor
        ctx.investigator
        "Discard a talent"
        [Choice (CardLabel c) [DiscardAsset c] | c <- talents]
 where
  isTalent c = maybe False ((== Talent) . (.assetType)) <$> assetDef c

taintedDiscardFocus :: EffectCtx -> GameM ()
taintedDiscardFocus ctx = discardAllFocus ctx.investigator

discardAllFocus :: InvestigatorId -> GameM ()
discardAllFocus iid = do
  i <- getInvestigator iid
  when (focusCount i > 0) do
    investigatorL iid . #focus .= mempty
    logText "Every focus token is discarded"

{- | "You suffer one direct damage and one direct horror. Then each other
investigator suffers two direct damage and two direct horror." Each investigator
takes their damage and horror as one plan, the way the card deals it.
-}
virulentPlague :: EffectCtx -> GameM ()
virulentPlague ctx = do
  others <- filter ((/= ctx.investigator) . (.id)) <$> playingInvestigators
  pushAll
    $ SufferHarm ctx.investigator ctx.source DirectHarm 1 1
    : [SufferHarm o.id ctx.source DirectHarm 2 2 | o <- others]
      <> [ResolveEffect ctx discardSelf]

{- | The back that is a monster: it fixes on whoever turned the card over and
cannot be shaken off (see 'holdsItsQuarry').
-}
grimSpectre :: EffectCtx -> GameM ()
grimSpectre ctx = do
  msid <- investigatorSpace ctx.investigator
  case msid of
    Nothing -> push (ResolveEffect ctx discardSelf)
    Just sid -> do
      cid <- newCard "grim-spectre"
      pushAll
        [ PlaceMonster cid sid Ready
        , SetMonsterPrey cid ctx.investigator
        , EngageMonster ctx.investigator cid
        , ResolveEffect ctx discardSelf
        ]

{- | The Vengeful Pursuer is only here for whoever was WANTED: with nobody held,
it has nothing to chase and goes.
-}
pursuerActivation :: CardId -> GameM ()
pursuerActivation mid = do
  m <- getMonster mid
  case m.state of
    Engaged (_ : _) -> pure ()
    _ -> do
      logText "The Vengeful Pursuer loses the trail"
      push (DiscardMonster mid)

{- | "It keeps to the investigator it haunts." The Grim Spectre takes no new prey:
it follows the one the DARK PACT set it on, and does nothing once it has them.
-}
spectreActivation :: CardId -> GameM ()
spectreActivation mid = do
  m <- getMonster mid
  d <- monsterDef mid
  case m.state of
    Engaged (_ : _) -> pure ()
    _ -> for_ m.prey (push . MonsterStep mid d.speed . TowardPrey . NamedInvestigator)
