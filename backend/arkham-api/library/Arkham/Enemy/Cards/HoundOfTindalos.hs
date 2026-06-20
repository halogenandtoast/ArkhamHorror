module Arkham.Enemy.Cards.HoundOfTindalos (houndOfTindalos) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Message.Lifted.Placement (place)
import Arkham.Placement
import Arkham.Trait (Trait (Future, Present, Scientist))

newtype HoundOfTindalos = HoundOfTindalos EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

houndOfTindalos :: EnemyCard HoundOfTindalos
houndOfTindalos = enemy HoundOfTindalos Cards.houndOfTindalos

instance HasModifiersFor HoundOfTindalos where
  getModifiersFor (HoundOfTindalos a) = do
    atPresent <- selectAny $ locationWithEnemy a.id <> LocationWithTrait Present
    atFuture <- selectAny $ locationWithEnemy a.id <> LocationWithTrait Future
    modifySelf a $ [HealthModifier 1 | atPresent] <> [HealthModifier 2 | atFuture]

instance HasAbilities HoundOfTindalos where
  getAbilities (HoundOfTindalos a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage HoundOfTindalos where
  runMessage msg e@(HoundOfTindalos attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> do
        scientists <- select $ AssetWithTrait Scientist <> AssetAt (LocationWithId lid)
        for_ scientists \scientist -> dealAssetHorror scientist (attrs.ability 1) 1
        tindalos <- selectOne $ locationIs Locations.tindalos
        for_ tindalos \tindalos' -> do
          for_ scientists \scientist -> place scientist (AtLocation tindalos')
          investigators <- select $ InvestigatorAt (LocationWithId lid)
          for_ investigators \iid -> moveTo (attrs.ability 1) iid tindalos'
      pure e
    _ -> HoundOfTindalos <$> liftRunMessage msg attrs
