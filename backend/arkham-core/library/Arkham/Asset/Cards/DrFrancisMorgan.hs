module Arkham.Asset.Cards.DrFrancisMorgan
  ( drFrancisMorgan
  , DrFrancisMorgan(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype DrFrancisMorgan = DrFrancisMorgan AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drFrancisMorgan :: AssetCard DrFrancisMorgan
drFrancisMorgan = ally DrFrancisMorgan Cards.drFrancisMorgan (4, 1)

instance HasAbilities DrFrancisMorgan where
  getAbilities (DrFrancisMorgan x) =
    [ restrictedAbility x 1 ControlsThis $ ReactionAbility
        (Matcher.EnemyDefeated Timing.After You AnyEnemy)
        (ExhaustCost $ toTarget x)
    ]

instance HasModifiersFor DrFrancisMorgan where
  getModifiersFor (InvestigatorTarget iid) (DrFrancisMorgan a) =
    pure [ toModifier a (SkillModifier SkillCombat 1) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance RunMessage DrFrancisMorgan where
  runMessage msg a@(DrFrancisMorgan attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> DrFrancisMorgan <$> runMessage msg attrs
