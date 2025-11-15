module Arkham.Treachery.Cards.DimensionalHypnosisB (dimensionalHypnosisB) where

import Arkham.Cost
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Trait (Trait (Extradimensional))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DimensionalHypnosisB = DimensionalHypnosisB TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalHypnosisB :: TreacheryCard DimensionalHypnosisB
dimensionalHypnosisB = treachery DimensionalHypnosisB Cards.dimensionalHypnosisB

instance HasModifiersFor DimensionalHypnosisB where
  getModifiersFor (DimensionalHypnosisB attrs) = case attrs.placement of
    AttachedToLocation lid ->
      modified_ attrs lid [AdditionalCostToLeave $ SkillTestCost (toSource attrs) #willpower (Fixed 3)]
    _ -> pure mempty

instance RunMessage DimensionalHypnosisB where
  runMessage msg t@(DimensionalHypnosisB attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <-
        select
          $ NearestLocationTo iid
          $ LocationWithoutTreachery
          $ mapOneOf
            treacheryIs
            [Cards.dimensionalHypnosisA, Cards.dimensionalHypnosisB, Cards.dimensionalHypnosisC]
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      cancelMovement attrs iid
      withLocationOf iid \loc -> do
        extradimenionalEnemies <- select $ NearestEnemyTo iid $ EnemyWithTrait Extradimensional
        chooseTargetM iid extradimenionalEnemies \e -> moveTowards attrs e loc
      toDiscardBy iid attrs attrs
      pure t
    _ -> DimensionalHypnosisB <$> liftRunMessage msg attrs
