module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.RiverDocksDusk (riverDocksDusk) where

import Arkham.Ability
import Arkham.Helpers.Location (addDirectConnection)
import Arkham.I18n
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype RiverDocksDusk = RiverDocksDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverDocksDusk :: LocationCard RiverDocksDusk
riverDocksDusk = symbolLabel $ location RiverDocksDusk Cards.riverDocksDusk 3 (PerPlayer 1)

instance HasAbilities RiverDocksDusk where
  getAbilities (RiverDocksDusk a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (locationIs Cards.unvisitedIsleDusk))
      $ actionAbilityWithCost (GroupResourceCost (Static 5) (be a))

instance RunMessage RiverDocksDusk where
  runMessage msg l@(RiverDocksDusk attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.unvisitedIsleDusk) >>= traverse_ \isle -> do
        addDirectConnection attrs isle
        selectEach (investigatorAt attrs) \iid -> chooseOneM iid $ withI18n do
          labeled "moveTo" $ moveTo (attrs.ability 1) iid isle
          labeled "doNotMove" nothing
      pure l
    _ -> RiverDocksDusk <$> liftRunMessage msg attrs
