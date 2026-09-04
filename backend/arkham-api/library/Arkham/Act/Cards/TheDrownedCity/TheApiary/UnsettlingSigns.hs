module Arkham.Act.Cards.TheDrownedCity.TheApiary.UnsettlingSigns (unsettlingSigns) where

import Arkham.Ability
import Arkham.Act.CardDefs.TheDrownedCity.TheApiary qualified as Acts
import Arkham.Act.CardDefs.TheDrownedCity.TheApiary qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TheDrownedCity.TheApiary qualified as Enemies
import Arkham.Enemy.CardDefs.TheDrownedCity.TheInescapable qualified as Enemies
import Arkham.Enemy.Creation (EnemyCreation, createExhausted)
import {-# SOURCE #-} Arkham.GameEnv (findAllCards)
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (
  getInvestigators,
  getPlayerCount,
  getSetAsideCardsMatching,
 )
import Arkham.Location.CardDefs.TheDrownedCity.TheApiary qualified as Locations
import Arkham.Location.Grid (GridLocation (..), Pos (..))
import Arkham.Location.Types (Field (LocationCardId))
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TheDrownedCity.TheApiary.Helpers
import Arkham.Trait (Trait (Cultist, Sanctum))

newtype UnsettlingSigns = UnsettlingSigns ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsettlingSigns :: ActCard UnsettlingSigns
unsettlingSigns = act (1, A) UnsettlingSigns Cards.unsettlingSigns Nothing

instance HasAbilities UnsettlingSigns where
  getAbilities (UnsettlingSigns x) =
    extend
      x
      [ restricted x 1 (exists $ InvestigatorWithClues $ atLeast 1) actionAbility
      , onlyOnce
          $ restricted
            x
            2
            (EachUndefeatedInvestigator $ at_ $ withTrait Sanctum)
          $ Objective
          $ freeTrigger (GroupClueCost (PerPlayer 2) Anywhere)
      ]

instance RunMessage UnsettlingSigns where
  runMessage msg a@(UnsettlingSigns attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- getInvestigators
      total <- getSpendableClueCount investigators
      scenarioI18n $ chooseAmount iid "cluesToSpend" "$clues" 1 total attrs
      pure a
    ResolveAmounts _iid (getChoiceAmount "$clues" -> cluesSpent) (isTarget attrs -> True) | cluesSpent > 0 -> do
      investigators <- getInvestigators
      spendCluesAsAGroup investigators cluesSpent
      n <- getPlayerCount
      let perClue = if n == 1 then 10 else 5
      lead <- getLead
      discardTopOfEncounterDeckAndHandle lead attrs (cluesSpent * perClue) attrs
      pure a
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let locationCards = filterLocations cards
      focusCards locationCards $ for_ locationCards (drawCard iid)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithClues attrs
      pure a
    -- The two Sanctum locations lead to the two halves of the scenario: the pilgrims
    -- waiting at the Lost Campsite, or Mother at the heart of the Nest.
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      eachInvestigator (`loseAllClues` attrs)
      atLostCampsite <- selectAny $ InvestigatorAt (locationIs Locations.lostCampsite)
      doStep (if atLostCampsite then 1 else 2) msg
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "lostPilgrims" do
      flavor do
        h "title"
        p "body"
        ul do
          li "flipApiaryEntrance"
          li "gatherCultists"
          li "mariaRivera"
          li "theInescapable"
          li "placeClues"
          li "continuePlaying"

      flipApiaryEntranceToDangerousExit

      -- "Search the encounter deck, discard pile, and all in- and out-of-play areas
      -- for each non-weakness Cultist enemy. Shuffle them together and place them in
      -- a facedown stack beneath Lost Campsite." Any already in play leave it first,
      -- so every copy ends up in the one stack.
      lostCampsite <- selectJust $ locationIs Locations.lostCampsite
      selectEach (EnemyWithTrait Cultist) (push . RemoveEnemy)
      cultists <- findAllCards (`cardMatch` (#enemy <> CardWithTrait Cultist <> NonWeakness))
      for_ cultists obtainCard
      placeUnderneath lostCampsite =<< shuffle =<< traverse (setFacedown True) cultists

      lead <- getLead
      maria <- getSetAsideCard Assets.mariaRivera
      investigators <- getInvestigators
      chooseOrRunOneM lead $ targets investigators (`takeControlOfSetAsideAsset` maria)

      spawnTheInescapable createExhausted
      placeCluesOnRevealedLocations attrs

      advanceToAct attrs Acts.lostPilgrims A
      pure a
    DoStep 2 (AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "theHiveMind" do
      flavor do
        h "title"
        p "body"
        ul do
          li "rearrangeRing"
          li "removeRemainingLocations"
          li "placeCentralChamber"
          li "spawnMother"
          li "theInescapable"
          li "placeClues"
          li "continuePlaying"

      -- The ring is Grasping Corridor above the Central Chamber and Acidic Coelom
      -- below it (the edge it faces), with the Apiary Entrance and Starving Corridor
      -- either side. Only the entrance has to move; the rest are already in place.
      let ring =
            [ Locations.apiaryEntranceBeckoningLight
            , Locations.graspingCorridor
            , Locations.starvingCorridor
            , Locations.acidicCoelom
            ]
      selectOne (locationIs Locations.apiaryEntranceBeckoningLight)
        >>= traverse_ (push . PlaceGrid . GridLocation (Pos (-1) (-2)))

      selectOne (locationIs Locations.acidicCoelom)
        >>= traverse_ (push . PlaceGrid . GridLocation (Pos 0 (-3)))

      -- 'removeLocation' already splits these the way the interlude asks: a victory
      -- location with no clues on it goes to the victory display, anything else is
      -- removed outright.
      selectEach (not_ $ mapOneOf locationIs ring) removeLocation
      strays <- findAllCards (`cardMatch` (#location <> not_ (mapOneOf cardIs ring)))
      for_ strays removeCardFromGame

      -- Revealed on placement: the interlude puts it into the ring face up, and its
      -- connection to the location it faces only exists while it is revealed.
      centralChamber <- placeSetAsideLocation Locations.centralChamber
      reveal centralChamber
      createSetAsideEnemy_ Enemies.mother centralChamber

      -- The set is only aside if the creature was not already defeated; if it was,
      -- setup removed it from the game.
      whenM (notNull <$> getSetAsideCardsMatching (cardIs Enemies.theInescapable))
        $ spawnTheInescapable id
      placeCluesOnRevealedLocations attrs

      advanceToAct attrs Acts.theHiveMind A
      pure a
    _ -> UnsettlingSigns <$> liftRunMessage msg attrs

-- | "Flip Apiary Entrance to its (Dangerous Exit) side."
flipApiaryEntranceToDangerousExit :: ReverseQueue m => m ()
flipApiaryEntranceToDangerousExit =
  selectOne (locationIs Locations.apiaryEntranceBeckoningLight) >>= traverse_ \lid -> do
    cardId <- field LocationCardId lid
    push $ ReplaceLocation lid (lookupCard Locations.apiaryEntranceDangerousExit cardId) Swap

{- | "Spawn the set-aside The Inescapable enemy at the location farthest from all
investigators. Shuffle the rest of The Inescapable encounter set into the encounter
deck, along with the encounter discard pile."
-}
spawnTheInescapable
  :: ReverseQueue m => (EnemyCreation Message -> EnemyCreation Message) -> m ()
spawnTheInescapable f = do
  lead <- getLead
  farthest <- select $ FarthestLocationFromAll Anywhere
  chooseOrRunOneM lead $ targets farthest \lid ->
    createSetAsideEnemyWith_ Enemies.theInescapable lid f
  shuffleEncounterDiscardBackIn
  -- The *rest* of the set: the enemy itself is spawning, and the spawn is deferred
  -- behind the choice above while the set-aside cards here are read immediately, so
  -- leaving it in would shuffle a second copy of it into the deck.
  shuffleSetAsideIntoDeck Deck.EncounterDeck
    $ CardFromEncounterSet Set.TheInescapable
    <> not_ (cardIs Enemies.theInescapable)

-- | "Place clues on each revealed location without victory X up to its clue value."
placeCluesOnRevealedLocations :: ReverseQueue m => ActAttrs -> m ()
placeCluesOnRevealedLocations attrs =
  selectEach (RevealedLocation <> not_ LocationWithVictory) (placeCluesUpToClueValue attrs)
