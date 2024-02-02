module Arkham.Act.Cards.WhatHappened (
  WhatHappened (..),
  whatHappened,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Lead))
import Arkham.Treachery.Cards qualified as Treacheries

newtype WhatHappened = WhatHappened ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

whatHappened :: ActCard WhatHappened
whatHappened = act (1, A) WhatHappened Cards.whatHappened Nothing

instance HasAbilities WhatHappened where
  getAbilities (WhatHappened x) =
    guard (onSide A x)
      *> [ mkAbility x 1 $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
         , restrictedAbility x 2 (AssetCount 2 $ AssetWithTrait Lead)
            $ Objective
            $ ForcedAbility AnyWindow
         ]

instance RunMessage WhatHappened where
  runMessage msg a@(WhatHappened attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DrawFromScenarioDeck iid LeadsDeck (toTarget attrs) 1
      pure a
    DrewFromScenarioDeck iid LeadsDeck (isTarget attrs -> True) (onlyEncounterCards -> cards) -> do
      pushAll $ InvestigatorDrewEncounterCard iid <$> cards
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      hasAlienDevice <- selectAny $ assetIs Assets.alienDevice
      hasManagersKey <- selectAny $ assetIs Assets.managersKey
      hasTomeOfRituals <- selectAny $ assetIs Assets.tomeOfRituals
      hasSinisterSolution <- selectAny $ assetIs Assets.sinisterSolution
      hasTimeWornLocket <- selectAny $ assetIs Assets.timeWornLocket

      cards <-
        genCards
          $ (guard hasAlienDevice *> replicate 3 Treacheries.encephalonSignal)
          <> (guard hasManagersKey *> replicate 3 Enemies.hotelSecurity)
          <> (guard hasTomeOfRituals *> replicate 3 Enemies.cultistOfTheEnclave)
          <> (guard hasSinisterSolution *> replicate 3 Treacheries.morbidAwareness)
          <> (guard hasTimeWornLocket *> replicate 3 Treacheries.chillingPresence)

      placements <-
        placeLocationCards_
          [ Locations.hotelRoof
          , Locations.room212
          , Locations.room245
          , Locations.officeMurderAtTheExcelsiorHotel
          , Locations.basement
          ]

      pushAll
        $ placements
        <> [ SetScenarioDeck LeadsDeck []
           , ShuffleCardsIntoDeck Deck.EncounterDeck cards
           , ShuffleEncounterDiscardBackIn
           , advanceActDeck attrs
           ]
      pure a
    _ -> WhatHappened <$> runMessage msg attrs
