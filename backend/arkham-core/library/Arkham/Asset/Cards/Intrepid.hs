module Arkham.Asset.Cards.Intrepid
  ( intrepid
  , Intrepid(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype Intrepid = Intrepid AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intrepid :: AssetCard Intrepid
intrepid = asset Intrepid Cards.intrepid

instance HasModifiersFor Intrepid where
  getModifiersFor (InvestigatorTarget iid) (Intrepid a) =
    pure $ if controlledBy a iid
      then toModifiers
        a
        [ SkillModifier SkillIntellect 1
        , SkillModifier SkillCombat 1
        , SkillModifier SkillAgility 1
        ]
      else []
  getModifiersFor _ _ = pure []

instance HasAbilities Intrepid where
  getAbilities (Intrepid a) =
    [restrictedAbility a 1 ControlsThis $ ForcedAbility $ RoundEnds Timing.When]

instance RunMessage Intrepid where
  runMessage msg a@(Intrepid attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Discard (toAbilitySource attrs 1) $ toTarget attrs
      pure a
    _ -> Intrepid <$> runMessage msg attrs
