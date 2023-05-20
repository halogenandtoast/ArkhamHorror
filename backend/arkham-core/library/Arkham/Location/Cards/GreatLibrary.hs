module Arkham.Location.Cards.GreatLibrary (
  greatLibrary,
  GreatLibrary (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype GreatLibrary = GreatLibrary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greatLibrary :: LocationCard GreatLibrary
greatLibrary = location GreatLibrary Cards.greatLibrary 2 (Static 4)

instance HasAbilities GreatLibrary where
  getAbilities (GreatLibrary attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 Here $
          ActionAbility Nothing $
            ActionCost 1
              <> ClueCost (PerPlayer 1)
      , restrictedAbility attrs 2 (CluesOnThis $ LessThan $ Static 4) $
          ForcedAbility $
            RoundEnds Timing.When
      ]

instance RunMessage GreatLibrary where
  runMessage msg l@(GreatLibrary attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid attrs iid SkillIntellect 3
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      current <- field LocationClues (toId attrs)
      push $ PlaceClues (toAbilitySource attrs 2) (toTarget attrs) (4 - current)
      pure l
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ Remember FoundTheProcess
        pure l
    _ -> GreatLibrary <$> runMessage msg attrs
