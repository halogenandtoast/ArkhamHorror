module Arkham.Location.Cards.RehearsalRoom (
  rehearsalRoom,
  RehearsalRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype RehearsalRoom = RehearsalRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

rehearsalRoom :: LocationCard RehearsalRoom
rehearsalRoom = location RehearsalRoom Cards.rehearsalRoom 1 (PerPlayer 1)

instance HasAbilities RehearsalRoom where
  getAbilities (RehearsalRoom attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
        $ ForcedAbility
        $ SkillTestResult
          Timing.After
          You
          (WhileInvestigating $ LocationWithId $ toId attrs)
          (SuccessResult $ AtLeast $ Static 2)
      | locationRevealed attrs
      ]

instance RunMessage RehearsalRoom where
  runMessage msg l@(RehearsalRoom attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> RehearsalRoom <$> runMessage msg attrs
