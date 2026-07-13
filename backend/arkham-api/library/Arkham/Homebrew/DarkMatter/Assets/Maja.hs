module Arkham.Homebrew.DarkMatter.Assets.Maja (maja) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype Maja = Maja AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maja :: AssetCard Maja
maja = asset Maja Cards.maja

-- "Investigators at any location may trigger this ability." -> NoRestriction so
-- the ability is offered to every investigator regardless of location. The
-- discard-3-from-hand is the ability cost, paid by the performing investigator.
instance HasAbilities Maja where
  getAbilities (Maja a) =
    [restricted a 1 NoRestriction $ FastAbility (HandDiscardCost 3 #any)]

instance RunMessage Maja where
  runMessage msg a@(Maja attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyEngagedWith iid
      for_ enemies $ disengageEnemy iid
      ls <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseOrRunOneM iid $ targets ls (moveTo (attrs.ability 1) iid)
      for_ enemies enemyCheckEngagement
      pure a
    _ -> Maja <$> liftRunMessage msg attrs
