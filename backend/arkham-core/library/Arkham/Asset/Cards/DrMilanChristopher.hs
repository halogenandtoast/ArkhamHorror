module Arkham.Asset.Cards.DrMilanChristopher where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype DrMilanChristopher = DrMilanChristopher AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drMilanChristopher :: AssetCard DrMilanChristopher
drMilanChristopher = ally DrMilanChristopher Cards.drMilanChristopher (1, 2)

instance HasModifiersFor DrMilanChristopher where
  getModifiersFor (InvestigatorTarget iid) (DrMilanChristopher a) =
    pure [toModifier a (SkillModifier SkillIntellect 1) | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities DrMilanChristopher where
  getAbilities (DrMilanChristopher x) =
    [ restrictedAbility x 1 ControlsThis $
        ReactionAbility
          ( SkillTestResult Timing.After You (WhileInvestigating Anywhere) $
              SuccessResult AnyValue
          )
          Free
    ]

instance RunMessage DrMilanChristopher where
  runMessage msg a@(DrMilanChristopher attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ TakeResources iid 1 (toAbilitySource attrs 1) False
      pure a
    _ -> DrMilanChristopher <$> runMessage msg attrs
