module Arkham.Treachery.Cards.WallOfThorns (wallOfThorns) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WallOfThorns = WallOfThorns TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wallOfThorns :: TreacheryCard WallOfThorns
wallOfThorns = treachery WallOfThorns Cards.wallOfThorns

instance HasAbilities WallOfThorns where
  getAbilities (WallOfThorns a) =
    [ restricted a 1 IsDay $ forced $ Enters #when You (locationWithTreachery a)
    , restricted a 2 IsNight $ forced $ Enters #when You (locationWithTreachery a)
    ]

instance RunMessage WallOfThorns where
  runMessage msg t@(WallOfThorns attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <- select $ NearestLocationTo iid $ LocationWithoutTreachery (treacheryIs Cards.wallOfThorns)
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 2
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    _ -> WallOfThorns <$> liftRunMessage msg attrs
