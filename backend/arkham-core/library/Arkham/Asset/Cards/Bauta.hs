module Arkham.Asset.Cards.Bauta (
  bauta,
  Bauta (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype Bauta = Bauta AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

bauta :: AssetCard Bauta
bauta = asset Bauta Cards.bauta

instance HasAbilities Bauta where
  getAbilities (Bauta a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (AssetEntersPlay Timing.After $ AssetWithId $ toId a)
          Free
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility
          ( InitiatedSkillTest
              Timing.When
              You
              (NotSkillType SkillCombat)
              AnySkillTestValue
              #any
          )
          (DiscardCost FromPlay $ toTarget a)
    ]

instance RunMessage Bauta where
  runMessage msg a@(Bauta attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ TakeResources iid 2 (toAbilitySource attrs 1) False
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      skillTest <- getJustSkillTest
      let
        newBase =
          case skillTestBaseValue skillTest of
            SkillBaseValue _ -> SkillBaseValue #combat
            AndSkillBaseValue _ -> SkillBaseValue #combat
            HalfResourcesOf x -> HalfResourcesOf x
            StaticBaseValue x -> StaticBaseValue x
      push $ ChangeSkillTestType (SkillSkillTest #combat) newBase
      pure a
    _ -> Bauta <$> runMessage msg attrs
