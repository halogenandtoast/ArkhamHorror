module Arkham.Location.Cards.DowntownArkhamAsylum (downtownArkhamAsylum) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards (downtownArkhamAsylum)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DowntownArkhamAsylum = DowntownArkhamAsylum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownArkhamAsylum :: LocationCard DowntownArkhamAsylum
downtownArkhamAsylum = location DowntownArkhamAsylum Cards.downtownArkhamAsylum 4 (PerPlayer 2)

instance HasAbilities DowntownArkhamAsylum where
  getAbilities (DowntownArkhamAsylum x) =
    extendRevealed1 x
      $ playerLimit PerGame
      $ restricted x 1 (Here <> exists (HealableInvestigator (toSource x) #horror You)) actionAbility

instance RunMessage DowntownArkhamAsylum where
  runMessage msg l@(DowntownArkhamAsylum attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenM (canHaveHorrorHealed (attrs.ability 1) iid) $ healHorror iid (attrs.ability 1) 3
      pure l
    _ -> DowntownArkhamAsylum <$> liftRunMessage msg attrs
