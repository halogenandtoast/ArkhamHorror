module Arkham.Asset.Assets.Knuckleduster (knuckleduster, Knuckleduster (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Keyword qualified as Keyword
import Arkham.Prelude

newtype Knuckleduster = Knuckleduster AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knuckleduster :: AssetCard Knuckleduster
knuckleduster = asset Knuckleduster Cards.knuckleduster

instance HasAbilities Knuckleduster where
  getAbilities (Knuckleduster a) = [fightAbility a 1 mempty ControlsThis]

instance HasModifiersFor Knuckleduster where
  getModifiersFor (Knuckleduster attrs) =
    getSkillTestTarget >>= \case
      Nothing -> pure mempty
      Just target -> case target.enemy of
        Nothing -> pure mempty
        Just eid -> maybeModified_ attrs eid do
          guardM $ isAbilitySource attrs 1 <$> MaybeT getSkillTestSource
          Action.Fight <- MaybeT getSkillTestAction
          pure [AddKeyword Keyword.Retaliate]

instance RunMessage Knuckleduster where
  runMessage msg a@(Knuckleduster attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      enabled <- skillTestModifier sid source iid (DamageDealt 1)
      pushAll [enabled, chooseFight]
      pure a
    _ -> Knuckleduster <$> runMessage msg attrs
