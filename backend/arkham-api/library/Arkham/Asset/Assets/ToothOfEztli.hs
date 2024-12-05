module Arkham.Asset.Assets.ToothOfEztli (toothOfEztli, ToothOfEztli (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest
import Arkham.Matcher

newtype ToothOfEztli = ToothOfEztli AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toothOfEztli :: AssetCard ToothOfEztli
toothOfEztli = asset ToothOfEztli Cards.toothOfEztli

instance HasModifiersFor ToothOfEztli where
  getModifiersFor (ToothOfEztli a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      source <- MaybeT getSkillTestSource
      _ <- hoistMaybe source.treachery
      pure [SkillModifier #willpower 1, SkillModifier #agility 1]

instance HasAbilities ToothOfEztli where
  getAbilities (ToothOfEztli x) =
    [ restricted x 1 ControlsThis
        $ ReactionAbility
          (SkillTestResult #after You (SkillTestOnTreachery AnyTreachery) (SuccessResult AnyValue))
          (exhaust x)
    ]

instance RunMessage ToothOfEztli where
  runMessage msg a@(ToothOfEztli attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> ToothOfEztli <$> liftRunMessage msg attrs
