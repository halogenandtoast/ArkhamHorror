module Arkham.Treachery.Cards.AntarcticWind (antarcticWind, AntarcticWind (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AntarcticWind = AntarcticWind TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

antarcticWind :: TreacheryCard AntarcticWind
antarcticWind = treachery AntarcticWind Cards.antarcticWind

instance HasModifiersFor AntarcticWind where
  getModifiersFor (AntarcticWind a) = case a.placement of
    AttachedToLocation lid -> modifySelect a (investigatorAt lid) [CannotDrawCards, CannotPlay AnyCard]
    _ -> pure mempty

instance HasAbilities AntarcticWind where
  getAbilities (AntarcticWind a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage AntarcticWind where
  runMessage msg t@(AntarcticWind attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <- select $ NearestLocationTo iid $ LocationWithoutTreachery (treacheryIs Cards.antarcticWind)
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> AntarcticWind <$> liftRunMessage msg attrs
