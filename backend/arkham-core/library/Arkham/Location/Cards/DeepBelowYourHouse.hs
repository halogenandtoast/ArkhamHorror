module Arkham.Location.Cards.DeepBelowYourHouse where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (deepBelowYourHouse)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype DeepBelowYourHouse = DeepBelowYourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

deepBelowYourHouse :: LocationCard DeepBelowYourHouse
deepBelowYourHouse =
  location DeepBelowYourHouse Cards.deepBelowYourHouse 4 (PerPlayer 1)

instance HasAbilities DeepBelowYourHouse where
  getAbilities (DeepBelowYourHouse attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage DeepBelowYourHouse where
  runMessage msg l@(DeepBelowYourHouse attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid attrs iid #agility 3
      pure l
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      pushAll $ replicate n $ findAndDrawEncounterCard iid $ cardIs Enemies.swarmOfRats
      pure l
    _ -> DeepBelowYourHouse <$> runMessage msg attrs
