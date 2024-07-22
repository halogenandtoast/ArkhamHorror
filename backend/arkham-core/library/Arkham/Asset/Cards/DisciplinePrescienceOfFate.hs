module Arkham.Asset.Cards.DisciplinePrescienceOfFate (
  disciplinePrescienceOfFate,
  disciplinePrescienceOfFateEffect,
  DisciplinePrescienceOfFate (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Effect.Runner hiding (createCardEffect)
import Arkham.Modifier

newtype DisciplinePrescienceOfFate = DisciplinePrescienceOfFate AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplinePrescienceOfFate :: AssetCard DisciplinePrescienceOfFate
disciplinePrescienceOfFate = asset DisciplinePrescienceOfFate Cards.disciplinePrescienceOfFate

instance HasModifiersFor DisciplinePrescienceOfFate where
  getModifiersFor (InvestigatorTarget iid) (DisciplinePrescienceOfFate a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #combat 1]
  getModifiersFor _ _ = pure []

instance HasAbilities DisciplinePrescienceOfFate where
  getAbilities (DisciplinePrescienceOfFate x) =
    [doesNotProvokeAttacksOfOpportunity $ restrictedAbility x 1 ControlsThis actionAbility]

instance RunMessage DisciplinePrescienceOfFate where
  runMessage msg a@(DisciplinePrescienceOfFate attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      createCardEffect Cards.disciplinePrescienceOfFate Nothing (attrs.ability 1) iid
      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure a
    _ -> DisciplinePrescienceOfFate <$> liftRunMessage msg attrs

newtype DisciplinePrescienceOfFateEffect = DisciplinePrescienceOfFateEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplinePrescienceOfFateEffect :: EffectArgs -> DisciplinePrescienceOfFateEffect
disciplinePrescienceOfFateEffect = cardEffect DisciplinePrescienceOfFateEffect Cards.disciplinePrescienceOfFate

instance HasModifiersFor DisciplinePrescienceOfFateEffect where
  getModifiersFor target@(InvestigatorTarget iid) (DisciplinePrescienceOfFateEffect a) | target == a.target = do
    getSkillTestInvestigator >>= \case
      Just iid' | iid == iid' -> pure $ toModifiers a [AnySkillValue 5]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage DisciplinePrescienceOfFateEffect where
  runMessage msg e@(DisciplinePrescienceOfFateEffect attrs) = runQueueT $ case msg of
    SkillTestEnded {} -> do
      case attrs.source of
        AbilitySource (AssetSource inner) _ -> case attrs.target of
          InvestigatorTarget iid -> flipOverBy iid attrs.source inner
          _ -> error "invalid target"
        AbilitySource (ProxySource (CardIdSource _) (AssetSource inner)) _ -> case attrs.target of
          InvestigatorTarget iid -> flipOverBy iid attrs.source inner
          _ -> error "invalid target"
        _ -> error "invalid source"
      push $ disable attrs
      pure e
    EndTurn {} -> do
      push $ disable attrs
      pure e
    _ -> DisciplinePrescienceOfFateEffect <$> liftRunMessage msg attrs
