module Arkham.Asset.Cards.MichaelLeigh5 (michaelLeigh5, MichaelLeigh5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype MichaelLeigh5 = MichaelLeigh5 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

michaelLeigh5 :: AssetCard MichaelLeigh5
michaelLeigh5 = ally MichaelLeigh5 Cards.michaelLeigh5 (3, 3)

instance HasModifiersFor MichaelLeigh5 where
  getModifiersFor (InvestigatorTarget iid) (MichaelLeigh5 a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #combat 1, SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities MichaelLeigh5 where
  getAbilities (MichaelLeigh5 a) =
    [ controlledAbility a 1 cardRestriction
        $ freeReaction (SkillTestResult #after You (WhileInvestigating Anywhere) $ SuccessResult AnyValue)
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility (PerformAction #when You #fight)
        $ assetUseCost a Evidence 1
        <> exhaust a
    ]
   where
    cardRestriction = if findWithDefault 0 Evidence (assetUses a) == 3 then Never else NoRestriction

instance RunMessage MichaelLeigh5 where
  runMessage msg a@(MichaelLeigh5 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AddUses (attrs.ability 1) (toId a) Evidence 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ skillTestModifier (toAbilitySource attrs 1) iid (DamageDealt 1)
      pure a
    _ -> MichaelLeigh5 <$> runMessage msg attrs
