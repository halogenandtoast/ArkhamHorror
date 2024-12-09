module Arkham.Location.Cards.CursedShores (CursedShores (..), cursedShores, cursedShoresEffect) where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Import
import Arkham.GameValue
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (cursedShores)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype CursedShores = CursedShores LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedShores :: LocationCard CursedShores
cursedShores = location CursedShores Cards.cursedShores 1 (Static 0)

instance HasAbilities CursedShores where
  getAbilities (CursedShores attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 Here actionAbility
      , mkAbility attrs 2 $ forced $ Leaves #when You $ be attrs
      ]

instance RunMessage CursedShores where
  runMessage msg l@(CursedShores attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      createCardEffect Cards.cursedShores Nothing attrs iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      skillCards <- fieldMap InvestigatorHand (filterCards (card_ #skill)) iid
      chooseOneM iid $ targets skillCards $ discardCard iid (attrs.ability 2)
      pure l
    _ -> CursedShores <$> liftRunMessage msg attrs

newtype CursedShoresEffect = CursedShoresEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedShoresEffect :: EffectArgs -> CursedShoresEffect
cursedShoresEffect = cardEffect CursedShoresEffect Cards.cursedShores

instance HasModifiersFor CursedShoresEffect where
  getModifiersFor (CursedShoresEffect a) =
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just _ -> modified_ a a.target [AnySkillValue 2]

instance RunMessage CursedShoresEffect where
  runMessage msg e@(CursedShoresEffect attrs) = runQueueT $ case msg of
    SkillTestEnds _ _ _ -> disableReturn e
    EndTurn iid | isTarget iid attrs.target -> disableReturn e
    _ -> CursedShoresEffect <$> liftRunMessage msg attrs
