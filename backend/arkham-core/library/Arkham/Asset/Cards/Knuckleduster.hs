module Arkham.Asset.Cards.Knuckleduster (knuckleduster, Knuckleduster (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Keyword qualified as Keyword
import Arkham.Prelude

newtype Knuckleduster = Knuckleduster AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knuckleduster :: AssetCard Knuckleduster
knuckleduster = asset Knuckleduster Cards.knuckleduster

instance HasAbilities Knuckleduster where
  getAbilities (Knuckleduster a) = [fightAbility a 1 mempty ControlsThis]

instance HasModifiersFor Knuckleduster where
  getModifiersFor (EnemyTarget eid) (Knuckleduster attrs) = do
    toModifiers attrs . toList <$> runMaybeT do
      guardM $ isTarget eid <$> MaybeT getSkillTestTarget
      guardM $ isAbilitySource attrs 1 <$> MaybeT getSkillTestSource
      Action.Fight <- MaybeT getSkillTestAction
      pure $ AddKeyword Keyword.Retaliate
  getModifiersFor _ _ = pure []

instance RunMessage Knuckleduster where
  runMessage msg a@(Knuckleduster attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifier source iid (DamageDealt 1), chooseFight]
      pure a
    _ -> Knuckleduster <$> runMessage msg attrs
