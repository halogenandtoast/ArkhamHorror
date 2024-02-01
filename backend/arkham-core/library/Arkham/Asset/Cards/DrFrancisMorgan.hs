module Arkham.Asset.Cards.DrFrancisMorgan (
  drFrancisMorgan,
  DrFrancisMorgan (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing

newtype DrFrancisMorgan = DrFrancisMorgan AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

drFrancisMorgan :: AssetCard DrFrancisMorgan
drFrancisMorgan = ally DrFrancisMorgan Cards.drFrancisMorgan (4, 1)

instance HasAbilities DrFrancisMorgan where
  getAbilities (DrFrancisMorgan x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          (Matcher.EnemyDefeated Timing.After You ByAny AnyEnemy)
          (exhaust x)
    ]

instance HasModifiersFor DrFrancisMorgan where
  getModifiersFor (InvestigatorTarget iid) (DrFrancisMorgan a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #combat 1]
  getModifiersFor _ _ = pure []

instance RunMessage DrFrancisMorgan where
  runMessage msg a@(DrFrancisMorgan attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> DrFrancisMorgan <$> runMessage msg attrs
