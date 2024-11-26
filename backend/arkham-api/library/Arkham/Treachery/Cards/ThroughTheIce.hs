module Arkham.Treachery.Cards.ThroughTheIce (throughTheIce, ThroughTheIce (..)) where

import Arkham.Cost
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ThroughTheIce = ThroughTheIce TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheIce :: TreacheryCard ThroughTheIce
throughTheIce = treachery ThroughTheIce Cards.throughTheIce

instance HasModifiersFor ThroughTheIce where
  getModifiersFor target (ThroughTheIce attrs) | treacheryOn attrs target = do
    toModifiers
      attrs
      [ AdditionalCostToLeave $ SkillTestCost (toSource attrs) #agility (Fixed 2)
      , AdditionalCostToEnter $ SkillTestCost (toSource attrs) #agility (Fixed 2)
      ]
  getModifiersFor _ _ = pure []

instance RunMessage ThroughTheIce where
  runMessage msg t@(ThroughTheIce attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <- select $ NearestLocationTo iid $ LocationWithoutTreachery (treacheryIs Cards.kindredMist)
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      movementModifier attrs iid CannotMove
      assignDamageAndHorror iid attrs 1 1
      toDiscardBy iid attrs attrs
      pure t
    _ -> ThroughTheIce <$> liftRunMessage msg attrs
