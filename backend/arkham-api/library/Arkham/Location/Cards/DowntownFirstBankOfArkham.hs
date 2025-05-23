module Arkham.Location.Cards.DowntownFirstBankOfArkham (downtownFirstBankOfArkham) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Location.Cards qualified as Cards (downtownFirstBankOfArkham)
import Arkham.Location.Import.Lifted

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownFirstBankOfArkham :: LocationCard DowntownFirstBankOfArkham
downtownFirstBankOfArkham = location DowntownFirstBankOfArkham Cards.downtownFirstBankOfArkham 3 (PerPlayer 1)

instance HasAbilities DowntownFirstBankOfArkham where
  getAbilities (DowntownFirstBankOfArkham a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> can.gain.resources You) actionAbility

instance RunMessage DowntownFirstBankOfArkham where
  runMessage msg l@(DowntownFirstBankOfArkham attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 3
      pure l
    _ -> DowntownFirstBankOfArkham <$> liftRunMessage msg attrs
