{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.DowntownFirstBankOfArkham_Arkham (downtownFirstBankOfArkham_Arkham) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (downtownFirstBankOfArkham_Arkham)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DowntownFirstBankOfArkham_Arkham = DowntownFirstBankOfArkham_Arkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownFirstBankOfArkham_Arkham :: LocationCard DowntownFirstBankOfArkham_Arkham
downtownFirstBankOfArkham_Arkham =
  location DowntownFirstBankOfArkham_Arkham Cards.downtownFirstBankOfArkham_Arkham 4 (PerPlayer 2)

instance HasAbilities DowntownFirstBankOfArkham_Arkham where
  getAbilities (DowntownFirstBankOfArkham_Arkham a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage DowntownFirstBankOfArkham_Arkham where
  runMessage msg l@(DowntownFirstBankOfArkham_Arkham attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure l
    _ -> DowntownFirstBankOfArkham_Arkham <$> liftRunMessage msg attrs
