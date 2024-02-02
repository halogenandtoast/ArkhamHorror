module Arkham.Asset.Cards.DrMilanChristopher where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype DrMilanChristopher = DrMilanChristopher AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

drMilanChristopher :: AssetCard DrMilanChristopher
drMilanChristopher = ally DrMilanChristopher Cards.drMilanChristopher (1, 2)

instance HasModifiersFor DrMilanChristopher where
  getModifiersFor (InvestigatorTarget iid) (DrMilanChristopher a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities DrMilanChristopher where
  getAbilities (DrMilanChristopher x) =
    [ restrictedAbility x 1 ControlsThis
        $ freeReaction (SkillTestResult #after You (WhileInvestigating Anywhere) $ SuccessResult AnyValue)
    ]

instance RunMessage DrMilanChristopher where
  runMessage msg a@(DrMilanChristopher attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ TakeResources iid 1 (toAbilitySource attrs 1) False
      pure a
    _ -> DrMilanChristopher <$> runMessage msg attrs
