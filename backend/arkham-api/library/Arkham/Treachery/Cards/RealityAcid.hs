module Arkham.Treachery.Cards.RealityAcid (realityAcid) where

import Arkham.Asset.Types (Field (AssetCard, AssetTokens))
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Helpers
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Investigator.Types (
  Field (InvestigatorClass, InvestigatorDeck, InvestigatorDiscard, InvestigatorHand),
 )
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message (pattern LoseAllResources)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Phase
import Arkham.Projection
import Arkham.Queue (QueueT)
import Arkham.RequestedChaosTokenStrategy
import Arkham.Scenario.Types (Field (ScenarioTokens))
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.SkillType
import Arkham.SlotType
import Arkham.Token
import Arkham.Trait hiding (Trait (Cultist, ElderThing, Supply))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RealityAcid = RealityAcid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityAcid :: TreacheryCard RealityAcid
realityAcid = treachery RealityAcid Cards.realityAcid

-- An in-play enemy is devoured: removed from play, its card placed beneath
-- Subject 8L-08. These helpers are top-level so they stay polymorphic enough to
-- be used both directly and as the body of a `targeting` choice.
devourEnemyCard :: ReverseQueue m => EnemyId -> m ()
devourEnemyCard e = do
  card <- field EnemyCard e
  push $ RemoveFromPlay (toSource e)
  devour [card]

devourTreacheryCard :: ReverseQueue m => TreacheryId -> m ()
devourTreacheryCard tr = do
  card <- field TreacheryCard tr
  push $ RemoveFromPlay (toSource tr)
  devour [card]

devourAssetCard :: ReverseQueue m => AssetId -> m ()
devourAssetCard a = do
  card <- field AssetCard a
  push $ RemovedFromPlay (toSource a)
  devour [card]

-- A card in a player's deck/hand/discard is obtained (pulled from its zone) and
-- then devoured beneath Subject 8L-08.
obtainAndDevour :: (ReverseQueue m, FetchCard c, IsCard c) => c -> m ()
obtainAndDevour c = obtainCard c >> devour [toCard c]

-- Choose one member of a non-empty set to devour, then return the consulted
-- tokens. If the set is empty the aspect cannot be devoured, so consult again.
chooseToDevour
  :: (ReverseQueue m, Targetable a)
  => InvestigatorId -> m () -> m () -> [a] -> (a -> QueueT Message m ()) -> m ()
chooseToDevour iid again done xs act = case xs of
  [] -> again
  _ -> chooseOneM iid (targets xs act) >> done

-- Devour every member of a set, then return the consulted tokens.
devourEach :: Monad m => m () -> m () -> [a] -> (a -> m ()) -> m ()
devourEach again done xs act = case xs of
  [] -> again
  _ -> traverse_ act xs >> done

instance RunMessage RealityAcid where
  runMessage msg t@(RealityAcid attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Subject 8L-08 devours a random aspect of reality: reveal two chaos
      -- tokens and consult the Reality Acid chart.
      push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal 2) SetAside
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      let source = attrs.ability 1

      -- An aspect was resolved: return the two consulted tokens to the bag.
      let done = unfocusChaosTokens >> resetChaosTokens source

      -- Returning to the chart for a fresh result: return the two consulted
      -- tokens, then reveal two new ones from the full bag and consult again.
      -- Used both for unlisted combinations and for any aspect that cannot be
      -- devoured.
      let
        again = do
          done
          push $ RequestChaosTokens source (Just iid) (Reveal 2) SetAside

      let baseSkill st = nextPhaseModifier MythosPhase source iid (BaseSkillOf st 0) >> done

      push $ FocusChaosTokens tokens

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
              handleTokenMatch p q = (p f1 && q f2) || (p f2 && q f1)

          let go
                -- {autoFail} + {skull}/{cultist}: the non-Elite enemy nearest to you.
                | handleTokenMatch (== AutoFail) isSkullCultist = do
                    enemies <- select $ NearestEnemyTo iid (NonEliteEnemy <> not_ EnemyWithVictory)
                    chooseToDevour iid again done enemies devourEnemyCard
                -- {autoFail} + {tablet}/{elderThing}: a treachery at your location.
                | handleTokenMatch (== AutoFail) isTabletElder = do
                    treacheries <- select $ TreacheryAt (locationWithInvestigator iid) <> not_ TreacheryWithVictory
                    chooseToDevour iid again done treacheries devourTreacheryCard
                -- {autoFail} + (-1 to -8): 1 horror and 1 damage from your investigator card.
                | handleTokenMatch (== AutoFail) isNeg = do
                    healDamage iid source 1
                    healHorror iid source 1
                    done
                -- {autoFail} + (+0/+1): your greatest flaw (a weakness in your deck).
                | handleTokenMatch (== AutoFail) zeroOrPlus = do
                    weaknesses <- select $ inDeckOf iid <> basic WeaknessCard
                    case weaknesses of
                      [] -> again
                      _ -> do
                        chooseOneM iid $ targets weaknesses \c -> do
                          obtainAndDevour c
                          push $ ShuffleDeck (Deck.InvestigatorDeck iid)
                        done
                -- {autoFail} + {elderSign}: the {autoFail} token just revealed.
                | handleTokenMatch (== AutoFail) (== ElderSign) = do
                    done
                    push $ RemoveChaosToken AutoFail
                -- +1 + {skull}/{cultist}: your caution. Resolve Reality Acid three more times.
                | handleTokenMatch (== PlusOne) isSkullCultist = replicateM_ 3 again
                -- +1 + {tablet}/{elderThing}: your ignorance. Discover 1 clue at your location.
                | handleTokenMatch (== PlusOne) isTabletElder = do
                    discoverAtYourLocation NotInvestigate iid source 1
                    done
                -- +1 + (-1 to -8): friendships. Investigators cannot commit to each others' tests.
                | handleTokenMatch (== PlusOne) isNeg = do
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
                        removeTokens source loc #clue toRemove
                        done
                -- 0 + (-1/-2): itself, and then regurgitates itself. Deal 1 damage to Subject 8L-08.
                | handleTokenMatch (== Zero) neg1or2 =
                    getSubject8L08 >>= \case
                      Nothing -> again
                      Just s -> do
                        nonAttackEnemyDamage (Just iid) source 1 s
                        done
                -- 0 + (-3): all damage from each Manifold enemy.
                | handleTokenMatch (== Zero) (== MinusThree) = do
                    enemies <- select $ EnemyWithTrait Manifold
                    case enemies of
                      [] -> again
                      _ -> do
                        for_ enemies \e -> push $ HealAllDamage (toTarget e) source
                        done
                -- 0 + (-4 to -8): all supplies, ammo, charges, and secrets among assets you control.
                | handleTokenMatch (== Zero) neg4to8 = do
                    let useTypes = [Supply, Ammo, Charge, Secret]
                    assets <- select $ assetControlledBy iid <> oneOf (map AssetWithUses useTypes)
                    case assets of
                      [] -> again
                      _ -> do
                        for_ assets \a -> for_ useTypes \u -> do
                          n <- fieldMap AssetTokens (countTokens u) a
                          removeTokens source a u n
                        done
                -- -1 + {skull}/{cultist}/{tablet}/{elderThing}: set a base skill to 0.
                | handleTokenMatch (== MinusOne) (== Skull) = baseSkill SkillWillpower
                | handleTokenMatch (== MinusOne) (== Cultist) = baseSkill SkillIntellect
                | handleTokenMatch (== MinusOne) (== Tablet) = baseSkill SkillCombat
                | handleTokenMatch (== MinusOne) (== ElderThing) = baseSkill SkillAgility
                -- -1 + -1: your versatility. Cannot play off-class cards this round.
                | handleTokenMatch (== MinusOne) (== MinusOne) = do
                    cls <- field InvestigatorClass iid
                    roundModifier source iid (CannotPlay (not_ (CardWithClass cls)))
                    done
                -- -2 + {skull}/{cultist}: the top 3 cards of your deck.
                | handleTokenMatch (== MinusTwo) isSkullCultist = do
                    cs <- fieldMap InvestigatorDeck (take 3 . unDeck) iid
                    devourEach again done cs obtainAndDevour
                -- -2 + {tablet}/{elderThing}: the top 3 cards of your discard pile.
                | handleTokenMatch (== MinusTwo) isTabletElder = do
                    cs <- fieldMap InvestigatorDiscard (take 3) iid
                    devourEach again done cs obtainAndDevour
                -- -2 + (-4 to -8): your party's teamwork. Each investigator loses 1 action.
                | handleTokenMatch (== MinusTwo) neg4to8 = do
                    iids <- allInvestigators
                    for_ iids \i -> loseActions i source 1
                    done
                -- -3 + {skull}/{cultist}/{tablet}/{elderThing}: a Talent, Connection, or Condition asset.
                | handleTokenMatch (== MinusThree) isSymbol = do
                    assets <- select $ assetControlledBy iid <> hasAnyTrait [Talent, Connection, Condition]
                    chooseToDevour iid again done assets devourAssetCard
                -- {skull} + {skull}: the highest-cost Ally asset you control.
                | handleTokenMatch (== Skull) (== Skull) = do
                    assets <- select $ AssetWithHighestPrintedCost (assetControlledBy iid <> withTrait Ally)
                    chooseToDevour iid again done assets devourAssetCard
                -- {skull} + {cultist}: all event cards in your hand.
                | handleTokenMatch (== Skull) (== Cultist) = do
                    cs <- fieldMap InvestigatorHand (filterCards EventType) iid
                    devourEach again done cs obtainAndDevour
                -- {skull} + {tablet}: all of your resources.
                | handleTokenMatch (== Skull) (== Tablet) = do
                    push $ LoseAllResources iid source
                    done
                -- {skull} + {elderThing}: all skill cards in your hand.
                | handleTokenMatch (== Skull) (== ElderThing) = do
                    cs <- fieldMap InvestigatorHand (filterCards SkillType) iid
                    devourEach again done cs obtainAndDevour
                -- {tablet} + {tablet}: your sense of urgency. You cannot move this round.
                | handleTokenMatch (== Tablet) (== Tablet) = do
                    roundModifier source iid CannotMove
                    done
                -- {tablet} + {elderThing}: your potential. Your skills cannot be increased this round.
                | handleTokenMatch (== Tablet) (== ElderThing) = do
                    roundModifiers source iid (map SkillCannotBeIncreased allSkills)
                    done
                -- {elderThing} + {elderThing}: your patience. Place 1 doom on the current agenda.
                | handleTokenMatch (== ElderThing) (== ElderThing) = do
                    placeDoomOnAgenda 1
                    done
                -- {elderSign} + {skull}/{cultist}: all Spell and Ritual assets you control.
                | handleTokenMatch (== ElderSign) isSkullCultist = do
                    assets <- select $ assetControlledBy iid <> hasAnyTrait [Spell, Ritual]
                    devourEach again done assets devourAssetCard
                -- {elderSign} + {tablet}/{elderThing}: all Item assets you control.
                | handleTokenMatch (== ElderSign) isTabletElder = do
                    assets <- select $ assetControlledBy iid <> withTrait Item
                    devourEach again done assets devourAssetCard
                -- {elderSign} + 0: one of your hands. 1 fewer hand slot for the rest of the game.
                | handleTokenMatch (== ElderSign) (== Zero) = do
                    gameModifier source iid (FewerSlots HandSlot 1)
                    done
                -- {elderSign} + (-2/-3): 1 countermeasure.
                | handleTokenMatch (== ElderSign) neg2or3 = do
                    countermeasures <- scenarioFieldMap ScenarioTokens (countTokens Resource)
                    if countermeasures <= 0
                      then again
                      else do
                        removeTokens source ScenarioTarget #resource 1
                        done
                -- Any other listed combination resolves an aspect that has no in-game
                -- representation (your voice, your house, your soul, etc.) or is not yet
                -- implemented; per the chart, reveal two new tokens and consult again.
                | otherwise = again
          go
        _ -> again
      pure t
    _ -> RealityAcid <$> liftRunMessage msg attrs
