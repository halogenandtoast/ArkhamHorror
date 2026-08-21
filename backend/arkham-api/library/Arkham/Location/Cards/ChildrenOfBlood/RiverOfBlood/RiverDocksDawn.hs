module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.RiverDocksDawn (riverDocksDawn) where

import Arkham.Ability
import Arkham.Helpers.Location (addDirectConnection)
import Arkham.I18n
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype RiverDocksDawn = RiverDocksDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverDocksDawn :: LocationCard RiverDocksDawn
riverDocksDawn = symbolLabel $ location RiverDocksDawn Cards.riverDocksDawn 3 (PerPlayer 1)

instance HasAbilities RiverDocksDawn where
  getAbilities (RiverDocksDawn a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (locationIs Cards.unvisitedIsleDawn))
      $ actionAbilityWithCost (GroupResourceCost (Static 3) (be a))

instance RunMessage RiverDocksDawn where
  runMessage msg l@(RiverDocksDawn attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.unvisitedIsleDawn) >>= traverse_ \isle -> do
        addDirectConnection attrs isle
        selectEach (investigatorAt attrs) \iid -> chooseOneM iid $ withI18n do
          labeled' "moveTo" $ moveTo (attrs.ability 1) iid isle
          labeled' "doNotMove" nothing
      pure l
    _ -> RiverDocksDawn <$> liftRunMessage msg attrs
