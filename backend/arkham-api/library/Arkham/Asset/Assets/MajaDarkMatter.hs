module Arkham.Asset.Assets.MajaDarkMatter (majaDarkMatter) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MajaDarkMatter = MajaDarkMatter AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

majaDarkMatter :: AssetCard MajaDarkMatter
majaDarkMatter = asset MajaDarkMatter Cards.majaDarkMatter

-- "Investigators at any location may trigger this ability." -> NoRestriction so
-- the ability is offered to every investigator regardless of location. The
-- discard-3-from-hand is the ability cost, paid by the performing investigator.
instance HasAbilities MajaDarkMatter where
  getAbilities (MajaDarkMatter a) =
    [restricted a 1 NoRestriction $ FastAbility (HandDiscardCost 3 #any)]

instance RunMessage MajaDarkMatter where
  runMessage msg a@(MajaDarkMatter attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyEngagedWith iid
      for_ enemies $ disengageEnemy iid
      ls <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseOrRunOneM iid $ targets ls (moveTo (attrs.ability 1) iid)
      for_ enemies enemyCheckEngagement
      pure a
    _ -> MajaDarkMatter <$> liftRunMessage msg attrs
