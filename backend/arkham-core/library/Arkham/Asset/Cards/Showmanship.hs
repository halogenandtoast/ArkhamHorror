module Arkham.Asset.Cards.Showmanship (
  showmanship,
  showmanshipEffect,
  Showmanship (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..), WindowType (EnterPlay))

newtype Showmanship = Showmanship AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

showmanship :: AssetCard Showmanship
showmanship =
  asset Showmanship Cards.showmanship

instance HasAbilities Showmanship where
  getAbilities (Showmanship a) =
    [restrictedAbility a 1 ControlsThis $ ReactionAbility (AssetEntersPlay Timing.After AnyAsset) Free]

toAsset :: [Window] -> AssetId
toAsset [] = error "missing asset"
toAsset (Window _ (EnterPlay (AssetTarget aid)) : _) = aid
toAsset (_ : xs) = toAsset xs

instance RunMessage Showmanship where
  runMessage msg a@(Showmanship attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (toAsset -> aid) _ -> do
      push $
        createCardEffect
          Cards.showmanship
          (Just $ EffectMetaTarget $ toTarget aid)
          (toSource attrs)
          (toTarget iid)
      pure a
    _ -> Showmanship <$> runMessage msg attrs

newtype ShowmanshipEffect = ShowmanshipEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

showmanshipEffect :: EffectArgs -> ShowmanshipEffect
showmanshipEffect = cardEffect ShowmanshipEffect Cards.showmanship

instance HasModifiersFor ShowmanshipEffect where
  getModifiersFor target (ShowmanshipEffect attrs) | effectTarget attrs == target = do
    mability <- listToMaybe <$> getActiveAbilities
    pure $ toModifiers attrs $ case (mability, effectMetadata attrs) of
      (Just ab, Just (EffectMetaTarget t))
        | sourceToTarget (abilitySource ab) == t ->
            [SkillModifier sType 2 | sType <- allSkills]
      _ -> []
  getModifiersFor _ _ = pure []

instance RunMessage ShowmanshipEffect where
  runMessage msg e@(ShowmanshipEffect attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId attrs)
    _ -> ShowmanshipEffect <$> runMessage msg attrs
