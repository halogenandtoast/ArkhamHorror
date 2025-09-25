module Arkham.Location.Cards.TrophyRoomSpectral (trophyRoomSpectral) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Projection
import Arkham.Scenarios.AtDeathsDoorstep.Helpers

newtype TrophyRoomSpectral = TrophyRoomSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trophyRoomSpectral :: LocationCard TrophyRoomSpectral
trophyRoomSpectral = location TrophyRoomSpectral Cards.trophyRoomSpectral 2 (PerPlayer 1)

instance HasAbilities TrophyRoomSpectral where
  getAbilities (TrophyRoomSpectral a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "trophyRoomSpectral.haunted" a 1

instance RunMessage TrophyRoomSpectral where
  runMessage msg l@(TrophyRoomSpectral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resources <- field InvestigatorResources iid
      let
        resourcesToLose = min resources 2
        horrorToTake = 2 - resourcesToLose
      loseResources iid (attrs.ability 1) resourcesToLose
      when (horrorToTake > 0) do
        assignHorror iid (attrs.ability 1) horrorToTake
      pure l
    _ -> TrophyRoomSpectral <$> liftRunMessage msg attrs
