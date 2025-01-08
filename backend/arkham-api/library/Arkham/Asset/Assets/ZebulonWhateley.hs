module Arkham.Asset.Assets.ZebulonWhateley (
  zebulonWhateley,
  ZebulonWhateley (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ZebulonWhateley = ZebulonWhateley AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zebulonWhateley :: AssetCard ZebulonWhateley
zebulonWhateley =
  allyWith ZebulonWhateley Cards.zebulonWhateley (1, 4) (isStoryL .~ True)

instance HasAbilities ZebulonWhateley where
  getAbilities (ZebulonWhateley x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          (SkillTestResult Timing.After You (SkillTestOnTreachery AnyTreachery) (SuccessResult AnyValue))
          (exhaust x)
    ]

instance HasModifiersFor ZebulonWhateley where
  getModifiersFor (ZebulonWhateley a) = controllerGets a [SkillModifier #willpower 1]

instance RunMessage ZebulonWhateley where
  runMessage msg a@(ZebulonWhateley attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> ZebulonWhateley <$> runMessage msg attrs
