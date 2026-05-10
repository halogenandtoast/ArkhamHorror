module Arkham.Location.Cards.RooflessRampart (rooflessRampart) where

import Arkham.Ability
import Arkham.Agenda.Types (Field (..))
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers

newtype RooflessRampart = RooflessRampart LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rooflessRampart :: LocationCard RooflessRampart
rooflessRampart = locationWith RooflessRampart Cards.rooflessRampart 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RooflessRampart where
  getAbilities (RooflessRampart a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ actionAbilityWithCost (SpendTokenKeyCost 2 #"-1")

instance RunMessage RooflessRampart where
  runMessage msg l@(RooflessRampart attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      aid <- selectJust AnyAgenda
      doom <- field AgendaDoom aid
      enemies <- select NonEliteEnemy
      treacheries <- select InPlayTreachery
      chooseOrRunOneM iid $ scenarioI18n $ scope "rooflessRampart" do
        when (doom > 0) do
          labeled' "removeDoom" $ removeDoom (attrs.ability 1) aid 1
        unless (null enemies) do
          labeled' "discardEnemy" do
            chooseTargetM iid enemies $ toDiscardBy iid (attrs.ability 1)

        unless (null treacheries) do
          labeled' "discardTreachery" do
            chooseTargetM iid treacheries $ toDiscardBy iid (attrs.ability 1)
      record TheTeamDiscoveredAnAncientVault
      pure l
    _ -> RooflessRampart <$> liftRunMessage msg attrs
