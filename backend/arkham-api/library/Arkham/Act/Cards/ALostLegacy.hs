module Arkham.Act.Cards.ALostLegacy (aLostLegacy) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher

newtype ALostLegacy = ALostLegacy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aLostLegacy :: ActCard ALostLegacy
aLostLegacy = act (1, A) ALostLegacy Cards.aLostLegacy Nothing

instance HasAbilities ALostLegacy where
  getAbilities (ALostLegacy x) =
    [ mkAbility x 1
        $ Objective
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 2) "Pearl Estate Ruins"
    ]

instance RunMessage ALostLegacy where
  runMessage msg a@(ALostLegacy attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #clues attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      horrorsInTheRock <-
        shuffle =<< getSetAsideCardsMatching (CardFromEncounterSet Set.HorrorsInTheRock <> #location)
      newLocations <-
        for (zip [Pos 0 (-1), Pos 1 0, Pos 2 (-1)] horrorsInTheRock) (uncurry placeLocationInGrid)

      setAsideLocations <-
        getSetAsideCardsMatching
          $ mapOneOf cardIs [Locations.saltChamber, Locations.larvalTunnel, Locations.crystalNursery]
      topThree <- take 3 . unDeck <$> getEncounterDeck
      for_ topThree obtainCard

      shuffled <- shuffle $ setAsideLocations <> map toCard topThree
      facedown <- traverse (setFacedown True) shuffled

      startingLocations <-
        select
          $ mapOneOf
            locationIs
            [Locations.pearlEstateRuins, Locations.ashenSlope, Locations.crystalGrove]

      for_ (zip (startingLocations <> newLocations) facedown) \(loc, card) ->
        placeUnderneath loc [card]

      advanceActDeck attrs
      pure a
    _ -> ALostLegacy <$> liftRunMessage msg attrs
