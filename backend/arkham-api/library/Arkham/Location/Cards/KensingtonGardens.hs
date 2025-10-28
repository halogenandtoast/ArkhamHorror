module Arkham.Location.Cards.KensingtonGardens (kensingtonGardens) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype KensingtonGardens = KensingtonGardens LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kensingtonGardens :: LocationCard KensingtonGardens
kensingtonGardens = symbolLabel $ location KensingtonGardens Cards.kensingtonGardens 2 (PerPlayer 1)

instance HasAbilities KensingtonGardens where
  getAbilities (KensingtonGardens a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> exists (HealableInvestigator (a.ability 1) #horror You))
          $ freeReaction
          $ CampaignEvent #after (Just You) "exposed[decoy]"
      , restricted a 2 Here
          $ forced
          $ CampaignEvent #after (Just You) "exposed[enemy]"
      ]

instance RunMessage KensingtonGardens where
  runMessage msg l@(KensingtonGardens attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid attrs 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid attrs 1
      pure l
    _ -> KensingtonGardens <$> liftRunMessage msg attrs
