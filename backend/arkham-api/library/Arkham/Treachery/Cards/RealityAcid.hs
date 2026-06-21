module Arkham.Treachery.Cards.RealityAcid (realityAcid, realityAcidEffect) where

import Arkham.Asset.Types (Field (AssetTokens))
import Arkham.CampaignLogKey (CampaignLogKey (YouHaveNoSoul))
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.Difficulty (Difficulty (..))
import Arkham.Effect.Import
import Arkham.Helpers
import Arkham.Helpers.FlavorText (chaosTokenImg, cols, flavor, p, scope, setTitle)
import Arkham.Helpers.Modifiers (modifyEach)
import Arkham.Helpers.Scenario (getDifficulty, getScenarioMetaKeyDefault, scenarioFieldMap)
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Investigator.Types (
  Field (InvestigatorClass, InvestigatorDeck, InvestigatorDiscard, InvestigatorHand),
 )
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message (pattern LoseAllResources)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Modifier
import Arkham.Phase
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Projection
import Arkham.Queue (QueueT)
import Arkham.RequestedChaosTokenStrategy
import Arkham.Scenario.Types (Field (ScenarioTokens))
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.SkillType
import Arkham.SlotType
import Arkham.Token
import Arkham.Trait hiding (Trait (Cultist, ElderThing, Evidence, Expert, Supply))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Data.Aeson.Key qualified as Key

newtype RealityAcid = RealityAcid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityAcid :: TreacheryCard RealityAcid
realityAcid = treachery RealityAcid Cards.realityAcid

-- Hand Subject 8L-08 a reference (enemy, treachery, asset, or a loose card by
-- id) to devour. The subject's own ScenarioSpecific "devour" handler removes the
-- entity from play and places the card beneath it. Polymorphic and top-level so
-- it works both directly and as the body of a `targeting` choice.
devourTarget :: (ReverseQueue m, Targetable a) => a -> m ()
devourTarget x = push $ ScenarioSpecific "devour" (toJSON (toTarget x))

-- Choose one member of a non-empty set to devour, then return the consulted
-- tokens. If the set is empty the aspect cannot be devoured, so consult again.
chooseToDevour
  :: (ReverseQueue m, Targetable a)
  => InvestigatorId -> m () -> m () -> m () -> [a] -> (a -> QueueT Message m ()) -> m ()
chooseToDevour iid again done showModal xs act = case xs of
  [] -> again
  _ -> showModal >> chooseOneM iid (targets xs act) >> done

-- Devour every member of a set, then return the consulted tokens.
devourEach :: Monad m => m () -> m () -> m () -> [a] -> (a -> m ()) -> m ()
devourEach again done showModal xs act = case xs of
  [] -> again
  _ -> showModal >> traverse_ act xs >> done

cardLevel :: Card -> Int
cardLevel = fromMaybe 0 . cdLevel . toCardDef

-- Devour level 1-5 cards of the investigator's choice with at least `remaining`
-- total levels, one at a time (running total), then finish the consult.
devourLevels :: ReverseQueue m => InvestigatorId -> Source -> Int -> [Card] -> m ()
devourLevels iid source remaining candidates
  | remaining <= 0 || null candidates = resetChaosTokens source
  | otherwise = chooseOneM iid $ targets candidates \c -> do
      devourTarget c
      devourLevels iid source (remaining - cardLevel c) (filter (/= c) candidates)

instance RunMessage RealityAcid where
  runMessage msg t@(RealityAcid attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Subject 8L-08 devours a random aspect of reality: reveal two chaos
      -- tokens and consult the Reality Acid chart. A debug option may force the
      -- next pair of tokens; it is consumed immediately so later Reality Acid
      -- draws return to normal randomness.
      debugFaces <- getScenarioMetaKeyDefault "debugRealityAcidTokens" []
      case debugFaces :: [ChaosTokenFace] of
        [face1, face2] -> do
          tokens <- traverse createChaosToken [face1, face2]
          push $ ScenarioSpecific "blobClearDebugRealityAcidTokens" Null
          push $ RequestedChaosTokens (attrs.ability 1) (Just iid) tokens
        _ -> push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal 2) SetAside
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      let source = attrs.ability 1

      -- An aspect was resolved: return the two consulted tokens to the bag.
      let done = resetChaosTokens source

      -- Show a modal with the two revealed tokens (rendered inline) and the
      -- outcome text for the matched chart row. Shown only on a real resolution.
      let faces = map (.face) tokens
      let
        showOutcome key = scenarioI18n $ flavor $ scope "realityAcid" do
          setTitle "title"
          cols $ for_ faces chaosTokenImg
          p "devours"
          scope "outcomes" $ p key

      -- Returning to the chart for a fresh result: return the two consulted
      -- tokens, then reveal two new ones from the full bag and consult again.
      -- Used both for unlisted combinations and for any aspect that cannot be
      -- devoured.
      let
        again = do
          done
          push $ RequestChaosTokens source (Just iid) (Reveal 2) SetAside

      let baseSkill key st = showOutcome key >> nextPhaseModifier MythosPhase source iid (BaseSkillOf st 0) >> done

      -- Your greatest flaw: search your deck for a weakness and devour it.
      let
        devourGreatestFlaw = do
          weaknesses <- select $ inDeckOf iid <> basic WeaknessCard
          case weaknesses of
            [] -> again
            _ -> do
              showOutcome "greatestFlaw"
              chooseOneM iid $ targets weaknesses \c -> do
                devourTarget c
                push $ ShuffleDeck (Deck.InvestigatorDeck iid)
              done
      let
        healFromInvestigator = do
          showOutcome "heal"
          ok <-
            selectAny $ InvestigatorWithId iid <> oneOf [InvestigatorWithAnyHorror, InvestigatorWithAnyDamage]
          removeTokens source iid #damage 1
          removeTokens source iid #horror 1
          if ok then done else again

      -- Several aspects on the chart have no game state to devour ("your voice",
      -- "your house", etc). Rather than rerolling forever, each can be devoured a
      -- single time (per investigator or per group), tracked in the scenario meta
      -- under `key`; once consumed it cannot be devoured again, so consult again.
      let
        recordConsumed keyT value = push $ ScenarioSpecific "blobSetMeta" (toJSON (keyT :: Text, value))
      let
        oncePerGroup keyT effect = do
          consumed <- getScenarioMetaKeyDefault (Key.fromText keyT) False
          if consumed
            then again
            else showOutcome keyT >> effect >> recordConsumed keyT (toJSON True) >> done
      let
        oncePerInvestigator keyT effect = do
          consumed <- getScenarioMetaKeyDefault (Key.fromText keyT) []
          if iid `elem` consumed
            then again
            else showOutcome keyT >> effect >> recordConsumed keyT (toJSON (iid : consumed)) >> done

      -- Increase the count of cards beneath Subject 8L-08 with a random, unowned
      -- player card (used for aspects like a player reference or mini card).
      let
        devourRandomUnownedCard = do
          let pool = filter ((`elem` [AssetType, EventType, SkillType]) . cdCardType) (toList allPlayerCards)
          for_ (nonEmpty pool) \defs -> do
            def <- sample defs
            card <- genPlayerCard def
            devour [toCard card]

      -- Generate Your House (Core #124) and devour it beneath the blob.
      let
        devourYourHouse = do
          card <- genEncounterCard Locations.yourHouse
          devour [toCard card]

      -- The chaos bag auto-focuses the tokens it reveals; clear that so they are
      -- only shown once, rendered inline in the outcome modal.
      unfocusChaosTokens

      case map (.face) tokens of
        [f1, f2] -> do
          let negVal = \case
                MinusOne -> Just (1 :: Int)
                MinusTwo -> Just 2
                MinusThree -> Just 3
                MinusFour -> Just 4
                MinusFive -> Just 5
                MinusSix -> Just 6
                MinusSeven -> Just 7
                MinusEight -> Just 8
                _ -> Nothing
          let isNeg = isJust . negVal
              neg4to8 f = maybe False (>= 4) (negVal f)
              neg1or2 f = negVal f `elem` [Just 1, Just 2]
              neg2or3 f = negVal f `elem` [Just 2, Just 3]
              isSkullCultist f = f == Skull || f == Cultist
              isTabletElder f = f == Tablet || f == ElderThing
              isSymbol f = f `elem` [Skull, Cultist, Tablet, ElderThing]
              zeroOrPlus f = f == Zero || f == PlusOne
              handleTokenMatch matchA matchB = (matchA f1 && matchB f2) || (matchA f2 && matchB f1)

          let go
                -- {elderSign} + {skull}/{cultist}: the non-Elite enemy nearest to you.
                | handleTokenMatch (== ElderSign) isSkullCultist = do
                    enemies <- select $ NearestEnemyTo iid (NonEliteEnemy <> not_ EnemyWithVictory)
                    chooseToDevour iid again done (showOutcome "nearestEnemy") enemies devourTarget
                -- {elderSign} + {tablet}/{elderThing}: a treachery at your location.
                | handleTokenMatch (== ElderSign) isTabletElder = do
                    treacheries <- select $ TreacheryAt (locationWithInvestigator iid) <> not_ TreacheryWithVictory
                    chooseToDevour iid again done (showOutcome "treachery") treacheries devourTarget
                -- {elderSign} + (-1 to -8): 1 horror and 1 damage from your investigator card.
                | handleTokenMatch (== ElderSign) isNeg = healFromInvestigator
                -- {elderSign} + (+0/+1): your greatest flaw (a weakness in your deck).
                | handleTokenMatch (== ElderSign) zeroOrPlus = devourGreatestFlaw
                -- {elderSign} + {autoFail}: the {elderSign} token just revealed.
                | handleTokenMatch (== ElderSign) (== AutoFail) = do
                    showOutcome "elderSignToken"
                    done
                    push $ RemoveChaosToken ElderSign
                -- +1 + {skull}/{cultist}: your caution. Resolve Reality Acid three more times.
                | handleTokenMatch (== PlusOne) isSkullCultist = showOutcome "caution" >> replicateM_ 3 again
                -- +1 + {tablet}/{elderThing}: your ignorance. Discover 1 clue at your location.
                | handleTokenMatch (== PlusOne) isTabletElder = do
                    showOutcome "ignorance"
                    discoverAtYourLocation NotInvestigate iid source 1
                    done
                -- +1 + (-1 to -8): friendships. Investigators cannot commit to each others' tests.
                | handleTokenMatch (== PlusOne) isNeg = do
                    showOutcome "friendships"
                    iids <- allInvestigators
                    for_ iids \i -> roundModifier source i CannotCommitToOtherInvestigatorsSkillTests
                    done
                -- 0 + 0: 1 per-investigator clues from your location.
                | handleTokenMatch (== Zero) (== Zero) = do
                    loc <- selectJust $ locationWithInvestigator iid
                    n <- getPlayerCount
                    cluesThere <- field LocationClues loc
                    let toRemove = min n cluesThere
                    if toRemove <= 0
                      then again
                      else do
                        showOutcome "locationClues"
                        removeTokens source loc #clue toRemove
                        done
                -- 0 + (-1/-2): itself, and then regurgitates itself. Deal 1 damage to Subject 8L-08.
                | handleTokenMatch (== Zero) neg1or2 =
                    getSubject8L08 >>= \case
                      Nothing -> again
                      Just s -> do
                        showOutcome "regurgitate"
                        nonAttackEnemyDamage (Just iid) source 1 s
                        done
                -- 0 + (-3): all damage from each Manifold enemy.
                | handleTokenMatch (== Zero) (== MinusThree) = do
                    enemies <- select $ EnemyWithTrait Manifold
                    case enemies of
                      [] -> again
                      _ -> do
                        showOutcome "manifold"
                        for_ enemies \e -> push $ HealAllDamage (toTarget e) source
                        done
                -- 0 + (-4 to -8): all supplies, ammo, charges, and secrets among assets you control.
                | handleTokenMatch (== Zero) neg4to8 = do
                    let useTypes = [Supply, Ammo, Charge, Secret]
                    assets <- select $ assetControlledBy iid <> oneOf (map AssetWithUses useTypes)
                    case assets of
                      [] -> again
                      _ -> do
                        showOutcome "uses"
                        for_ assets \a -> for_ useTypes \u -> do
                          n <- fieldMap AssetTokens (countTokens u) a
                          removeTokens source a u n
                        done
                -- -1 + {skull}/{cultist}/{tablet}/{elderThing}: set a base skill to 0.
                | handleTokenMatch (== MinusOne) (== Skull) = baseSkill "willpower" SkillWillpower
                | handleTokenMatch (== MinusOne) (== Cultist) = baseSkill "intellect" SkillIntellect
                | handleTokenMatch (== MinusOne) (== Tablet) = baseSkill "combat" SkillCombat
                | handleTokenMatch (== MinusOne) (== ElderThing) = baseSkill "agility" SkillAgility
                -- -1 + -1: your versatility. Cannot play off-class cards this round.
                | handleTokenMatch (== MinusOne) (== MinusOne) = do
                    showOutcome "versatility"
                    cls <- field InvestigatorClass iid
                    roundModifier source iid (CannotPlay (not_ (CardWithClass cls)))
                    done
                -- -1 + (-4 to -8): devour level 1-5 cards of your choice with >= 5 total levels.
                | handleTokenMatch (== MinusOne) neg4to8 = do
                    pool <-
                      select
                        $ oneOf [inDeckOf iid, inDiscardOf iid, inHandOf NotForPlay iid, inPlayAreaOf iid]
                    let candidates = filter (\c -> let l = cardLevel c in l >= 1 && l <= 5) pool
                    if sum (map cardLevel candidates) < 5
                      then again
                      else showOutcome "levelCards" >> devourLevels iid source 5 candidates
                -- -2 + {skull}/{cultist}: the top 3 cards of your deck.
                | handleTokenMatch (== MinusTwo) isSkullCultist = do
                    cs <- fieldMap InvestigatorDeck (take 3 . unDeck) iid
                    devourEach again done (showOutcome "top3Deck") cs devourTarget
                -- -2 + {tablet}/{elderThing}: the top 3 cards of your discard pile.
                | handleTokenMatch (== MinusTwo) isTabletElder = do
                    cs <- fieldMap InvestigatorDiscard (take 3) iid
                    devourEach again done (showOutcome "top3Discard") cs devourTarget
                -- -2 + (-4 to -8): your party's teamwork. Each investigator loses 1 action.
                | handleTokenMatch (== MinusTwo) neg4to8 = do
                    showOutcome "teamwork"
                    iids <- allInvestigators
                    for_ iids \i -> loseActions i source 1
                    done
                -- -3 + {skull}/{cultist}/{tablet}/{elderThing}: a Talent, Connection, or Condition asset.
                | handleTokenMatch (== MinusThree) isSymbol = do
                    assets <- select $ assetControlledBy iid <> hasAnyTrait [Talent, Connection, Condition]
                    chooseToDevour iid again done (showOutcome "talentAsset") assets devourTarget
                -- -3 + (-4 to -8): your sense of time. Block abilities on time/watch/chrono cards.
                | handleTokenMatch (== MinusThree) neg4to8 = do
                    showOutcome "senseOfTime"
                    iids <- allInvestigators
                    let
                      timeCards =
                        oneOf
                          [ CardWithTitleContaining "time"
                          , CardWithTitleContaining "watch"
                          , CardWithTitleContaining "chrono"
                          ]
                    for_ iids \i -> roundModifier source i (CannotTriggerAbilityMatching (AbilityOnCard timeCards))
                    done
                -- {skull} + {skull}: the highest-cost Ally asset you control.
                | handleTokenMatch (== Skull) (== Skull) = do
                    assets <- select $ AssetWithHighestPrintedCost (assetControlledBy iid <> withTrait Ally)
                    chooseToDevour iid again done (showOutcome "ally") assets devourTarget
                -- {skull} + {cultist}: all event cards in your hand.
                | handleTokenMatch (== Skull) (== Cultist) = do
                    cs <- fieldMap InvestigatorHand (filterCards EventType) iid
                    devourEach again done (showOutcome "events") cs devourTarget
                -- {skull} + {tablet}: all of your resources.
                | handleTokenMatch (== Skull) (== Tablet) = do
                    showOutcome "resources"
                    push $ LoseAllResources iid source
                    done
                -- {skull} + {elderThing}: all skill cards in your hand.
                | handleTokenMatch (== Skull) (== ElderThing) = do
                    cs <- fieldMap InvestigatorHand (filterCards SkillType) iid
                    devourEach again done (showOutcome "skills") cs devourTarget
                -- {tablet} + {tablet}: your sense of urgency. You cannot move this round.
                | handleTokenMatch (== Tablet) (== Tablet) = do
                    showOutcome "cannotMove"
                    roundModifier source iid CannotMove
                    done
                -- {tablet} + {elderThing}: your potential. Your skills cannot be increased this round.
                | handleTokenMatch (== Tablet) (== ElderThing) = do
                    showOutcome "potential"
                    roundModifiers source iid (map SkillCannotBeIncreased allSkills)
                    done
                -- {elderThing} + {elderThing}: your patience. Place 1 doom on the current agenda.
                | handleTokenMatch (== ElderThing) (== ElderThing) = do
                    showOutcome "doom"
                    placeDoomOnAgenda 1
                    done
                -- {cultist} + {cultist}: all cards that have been exiled this scenario.
                | handleTokenMatch (== Cultist) (== Cultist) = do
                    exiled <- getScenarioMetaKeyDefault "exiledCards" []
                    case exiled :: [Card] of
                      [] -> again
                      _ -> do
                        showOutcome "exiledCards"
                        devour exiled
                        recordConsumed "exiledCards" (toJSON ([] :: [Card]))
                        done
                -- {cultist} + {tablet}: your identity. Treat your text box (and Traits) as blank this round.
                | handleTokenMatch (== Cultist) (== Tablet) = do
                    showOutcome "identity"
                    roundModifier source iid Blank
                    done
                -- {cultist} + {elderThing}: the concept of speed. Investigators cannot gain or take
                -- additional actions this round.
                | handleTokenMatch (== Cultist) (== ElderThing) = do
                    showOutcome "speed"
                    iids <- allInvestigators
                    for_ iids \i -> roundModifier source i CannotGainAdditionalActions
                    done
                -- {autoFail} + {skull}/{cultist}: all Spell and Ritual assets you control.
                | handleTokenMatch (== AutoFail) isSkullCultist = do
                    assets <- select $ assetControlledBy iid <> hasAnyTrait [Spell, Ritual]
                    devourEach again done (showOutcome "spellRitual") assets devourTarget
                -- {autoFail} + {tablet}/{elderThing}: all Item assets you control.
                | handleTokenMatch (== AutoFail) isTabletElder = do
                    assets <- select $ assetControlledBy iid <> withTrait Item
                    devourEach again done (showOutcome "item") assets devourTarget
                -- {autoFail} + (+1): the concept of a "discard pile". Until the end of the next
                -- mythos phase, cards that would be discarded are devoured instead.
                | handleTokenMatch (== AutoFail) (== PlusOne) =
                    getSubject8L08 >>= \case
                      Nothing -> again
                      Just s -> do
                        showOutcome "discardPile"
                        iids <- allInvestigators
                        for_ iids \i ->
                          nextPhaseModifier MythosPhase source i (PlaceUnderneathInsteadOfDiscard (toTarget s))
                        done
                -- {autoFail} + 0: one of your hands. 1 fewer hand slot for the rest of the game.
                | handleTokenMatch (== AutoFail) (== Zero) = do
                    showOutcome "handSlot"
                    gameModifier source iid (FewerSlots HandSlot 1)
                    done
                -- {autoFail} + (-2/-3): 1 countermeasure.
                | handleTokenMatch (== AutoFail) neg2or3 = do
                    countermeasures <- scenarioFieldMap ScenarioTokens (countTokens Resource)
                    if countermeasures <= 0
                      then again
                      else do
                        showOutcome "countermeasure"
                        removeTokens source ScenarioTarget #resource 1
                        done
                -- {autoFail} + (-4 to -8): the concept of easiness. Flip the scenario reference
                -- to its Hard/Expert side (only possible on the easy side).
                | handleTokenMatch (== AutoFail) neg4to8 = do
                    difficulty <- getDifficulty
                    if difficulty `elem` [Hard, Expert]
                      then again
                      else do
                        showOutcome "easiness"
                        push $ ScenarioSpecific "blobFlipToHard" Null
                        done
                -- {autoFail} + (-1): the concept of success. The next time an investigator
                -- would succeed at a test by 2 or more, they automatically fail instead.
                | handleTokenMatch (== AutoFail) (== MinusOne) = do
                    showOutcome "success"
                    createCardEffect Cards.realityAcid Nothing source iid
                    done
                -- +1 + 0: your player reference card (once per investigator; bumps the count under the blob).
                | handleTokenMatch (== PlusOne) (== Zero) =
                    oncePerInvestigator "playerReferenceCard" devourRandomUnownedCard
                -- (-4 to -8) + {skull}/{cultist}: your investigator mini card (once per investigator).
                | handleTokenMatch neg4to8 isSkullCultist =
                    oncePerInvestigator "miniCard" do
                      devourRandomUnownedCard
                      devouredMiniCards <- getScenarioMetaKeyDefault "devouredMiniCards" []
                      recordConsumed "devouredMiniCards" (toJSON (iid : devouredMiniCards :: [InvestigatorId]))
                -- (-4 to -8) + {tablet}/{elderThing}: your house (Core #124), devoured once for the group.
                | handleTokenMatch neg4to8 isTabletElder =
                    oncePerGroup "yourHouse" devourYourHouse
                -- (-4 to -8) + (-4 to -8): your soul. Record (once) that you have no soul.
                | handleTokenMatch neg4to8 neg4to8 =
                    oncePerGroup "noSoul" (record YouHaveNoSoul)
                -- 0 + {skull}: your voice (once per investigator).
                | handleTokenMatch (== Zero) (== Skull) =
                    oncePerInvestigator "voice" (pure ())
                -- 0 + {cultist}: your group's food and drinks (once for the whole group).
                | handleTokenMatch (== Zero) (== Cultist) =
                    oncePerGroup "foodAndDrinks" (pure ())
                -- 0 + {tablet}: the concept of language (once for the whole group).
                | handleTokenMatch (== Zero) (== Tablet) =
                    oncePerGroup "language" (pure ())
                -- 0 + {elderThing}: light (once per investigator).
                | handleTokenMatch (== Zero) (== ElderThing) =
                    oncePerInvestigator "light" (recordConsumed "lightActive" (toJSON True))
                -- -1 + -2: your cell phone (once per investigator).
                | handleTokenMatch (== MinusOne) (== MinusTwo) =
                    oncePerInvestigator "cellPhone" (pure ())
                -- -1 + -3: the chaos bag (once per investigator).
                | handleTokenMatch (== MinusOne) (== MinusThree) =
                    oncePerInvestigator "chaosBag" (pure ())
                -- -2 + -2: your deckbox (once per investigator).
                | handleTokenMatch (== MinusTwo) (== MinusTwo) =
                    oncePerInvestigator "deckbox" (pure ())
                -- -2 + -3: your card sleeve (once per investigator).
                | handleTokenMatch (== MinusTwo) (== MinusThree) =
                    oncePerInvestigator "cardSleeve" (pure ())
                -- Any other listed combination is either a deferred aspect or one with
                -- no in-game representation; per the chart, reveal two new tokens and
                -- consult again.
                | otherwise = again
          go
        _ -> again
      pure t
    _ -> RealityAcid <$> liftRunMessage msg attrs

-- The "concept of success": until the next skill test ends, a test that would
-- succeed by 2 or more fails instead.
newtype RealityAcidEffect = RealityAcidEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityAcidEffect :: EffectArgs -> RealityAcidEffect
realityAcidEffect = cardEffect RealityAcidEffect Cards.realityAcid

instance HasModifiersFor RealityAcidEffect where
  getModifiersFor (RealityAcidEffect attrs) =
    getSkillTestId >>= \case
      Nothing -> pure ()
      Just sid -> modifyEach attrs [SkillTestTarget sid] [AutomaticallyFailIfSucceedByAtLeast 2]

instance RunMessage RealityAcidEffect where
  runMessage msg e@(RealityAcidEffect attrs) = runQueueT $ case msg of
    SkillTestEnds {} -> disableReturn e
    _ -> RealityAcidEffect <$> liftRunMessage msg attrs
