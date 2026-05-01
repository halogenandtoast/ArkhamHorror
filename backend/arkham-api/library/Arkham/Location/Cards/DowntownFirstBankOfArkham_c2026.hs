module Arkham.Location.Cards.DowntownFirstBankOfArkham_c2026 (downtownFirstBankOfArkham_c2026) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (downtownFirstBankOfArkham_c2026)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DowntownFirstBankOfArkham_c2026 = DowntownFirstBankOfArkham_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownFirstBankOfArkham_c2026 :: LocationCard DowntownFirstBankOfArkham_c2026
downtownFirstBankOfArkham_c2026 =
  location DowntownFirstBankOfArkham_c2026 Cards.downtownFirstBankOfArkham_c2026 4 (PerPlayer 2)

instance HasAbilities DowntownFirstBankOfArkham_c2026 where
  getAbilities (DowntownFirstBankOfArkham_c2026 a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage DowntownFirstBankOfArkham_c2026 where
  runMessage msg l@(DowntownFirstBankOfArkham_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure l
    _ -> DowntownFirstBankOfArkham_c2026 <$> liftRunMessage msg attrs
