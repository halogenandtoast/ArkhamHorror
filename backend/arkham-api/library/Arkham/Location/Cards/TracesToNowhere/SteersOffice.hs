module Arkham.Location.Cards.TracesToNowhere.SteersOffice (steersOffice) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Arkham))

newtype SteersOffice = SteersOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

steersOffice :: LocationCard SteersOffice
steersOffice = location SteersOffice Cards.steersOffice 2 (PerPlayer 1)

instance HasAbilities SteersOffice where
  getAbilities (SteersOffice a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> ScenarioDeckWithCard LeadsDeck)
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (be a)
      , restricted a 2 Here actionAbility
      ]

instance RunMessage SteersOffice where
  runMessage msg l@(SteersOffice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawFromLeadsDeck iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <-
        select
          $ RevealedLocation
          <> LocationWithTrait Arkham
          <> not_ (LocationWithInvestigator Anyone)
      n <- perPlayer 1
      chooseTargetM iid locations \target ->
        push $ PlaceCluesUpToClueValue target (attrs.ability 2) n
      pure l
    _ -> SteersOffice <$> liftRunMessage msg attrs
