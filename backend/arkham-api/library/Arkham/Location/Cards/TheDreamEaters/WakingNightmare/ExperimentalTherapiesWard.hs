module Arkham.Location.Cards.TheDreamEaters.WakingNightmare.ExperimentalTherapiesWard (
  experimentalTherapiesWard,
  ExperimentalTherapiesWard (..),
)
where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.CardDefs.TheDreamEaters.WakingNightmare qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype ExperimentalTherapiesWard = ExperimentalTherapiesWard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

experimentalTherapiesWard :: LocationCard ExperimentalTherapiesWard
experimentalTherapiesWard = location ExperimentalTherapiesWard Cards.experimentalTherapiesWard 4 (PerPlayer 2)

instance HasAbilities ExperimentalTherapiesWard where
  getAbilities (ExperimentalTherapiesWard attrs) =
    extendRevealed
      attrs
      [ mkAbility attrs 1
          $ triggered
            (InitiatedSkillTest #when You #any #any (WhileInvestigating $ be attrs))
            (HorrorCost (toSource attrs) YouTarget 1)
      ]

instance RunMessage ExperimentalTherapiesWard where
  runMessage msg l@(ExperimentalTherapiesWard attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier sid (toAbilitySource attrs 1) attrs (ShroudModifier (-2))
      pure l
    _ -> ExperimentalTherapiesWard <$> liftRunMessage msg attrs
