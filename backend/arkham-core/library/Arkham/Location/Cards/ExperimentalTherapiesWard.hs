module Arkham.Location.Cards.ExperimentalTherapiesWard (
  experimentalTherapiesWard,
  ExperimentalTherapiesWard (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype ExperimentalTherapiesWard = ExperimentalTherapiesWard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

experimentalTherapiesWard :: LocationCard ExperimentalTherapiesWard
experimentalTherapiesWard = location ExperimentalTherapiesWard Cards.experimentalTherapiesWard 4 (PerPlayer 2)

instance HasAbilities ExperimentalTherapiesWard where
  getAbilities (ExperimentalTherapiesWard attrs) =
    withRevealedAbilities
      attrs
      [ mkAbility attrs 1
          $ ReactionAbility
            (InitiatedSkillTest #when You #any #any (WhileInvestigating $ be attrs))
            (HorrorCost (toSource attrs) YouTarget 1)
      ]

instance RunMessage ExperimentalTherapiesWard where
  runMessage msg l@(ExperimentalTherapiesWard attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ skillTestModifier sid (toAbilitySource attrs 1) attrs (ShroudModifier (-2))
      pure l
    _ -> ExperimentalTherapiesWard <$> runMessage msg attrs
