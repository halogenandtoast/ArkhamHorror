module Arkham.Location.Cards.DowntownArkhamSanatorium (downtownArkhamSanatorium) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (downtownArkhamSanatorium)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DowntownArkhamSanatorium = DowntownArkhamSanatorium LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownArkhamSanatorium :: LocationCard DowntownArkhamSanatorium
downtownArkhamSanatorium = location DowntownArkhamSanatorium Cards.downtownArkhamSanatorium 4 (PerPlayer 2)

instance HasAbilities DowntownArkhamSanatorium where
  getAbilities (DowntownArkhamSanatorium a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage DowntownArkhamSanatorium where
  runMessage msg l@(DowntownArkhamSanatorium attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select (investigatorAt attrs)
      allies <- select (AllyAsset <> assetAt attrs)
      chooseOrRunOneM iid do
        targets investigators \iid' -> healHorror iid' (attrs.ability 1) 1
        targets allies \aid -> healHorror aid (attrs.ability 1) 1
      pure l
    _ -> DowntownArkhamSanatorium <$> liftRunMessage msg attrs
