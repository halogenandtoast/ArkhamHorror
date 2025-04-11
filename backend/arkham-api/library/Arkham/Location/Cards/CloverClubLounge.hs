module Arkham.Location.Cards.CloverClubLounge (cloverClubLounge) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (cloverClubLounge)
import Arkham.Location.Import.Lifted

newtype CloverClubLounge = CloverClubLounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubLounge :: LocationCard CloverClubLounge
cloverClubLounge = symbolLabel $ location CloverClubLounge Cards.cloverClubLounge 2 (Static 0)

instance HasAbilities CloverClubLounge where
  getAbilities (CloverClubLounge attrs) =
    extendRevealed1 attrs
      $ limitedAbility (PlayerLimit PerGame 1)
      $ restricted attrs 1 (OnAct 1)
      $ actionAbilityWithCost (HandDiscardCost 1 $ #ally <> #asset)

instance RunMessage CloverClubLounge where
  runMessage msg l@(CloverClubLounge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> CloverClubLounge <$> liftRunMessage msg attrs
