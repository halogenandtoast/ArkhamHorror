module Arkham.Treachery.Cards.DimensionalHypnosisA (dimensionalHypnosisA) where

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

newtype DimensionalHypnosisA = DimensionalHypnosisA TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalHypnosisA :: TreacheryCard DimensionalHypnosisA
dimensionalHypnosisA = treachery DimensionalHypnosisA Cards.dimensionalHypnosisA

instance HasModifiersFor DimensionalHypnosisA where
  getModifiersFor (DimensionalHypnosisA attrs) = case attrs.placement of
    AttachedToLocation lid ->
      modified_ attrs lid [AdditionalCostToLeave $ SkillTestCost (toSource attrs) #willpower (Fixed 3)]
    _ -> pure mempty

instance RunMessage DimensionalHypnosisA where
  runMessage msg t@(DimensionalHypnosisA attrs) = runQueueT $ case msg of
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
    _ -> DimensionalHypnosisA <$> liftRunMessage msg attrs
