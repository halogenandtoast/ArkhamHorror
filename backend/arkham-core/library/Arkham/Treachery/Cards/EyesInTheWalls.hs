module Arkham.Treachery.Cards.EyesInTheWalls (
  eyesInTheWalls,
  EyesInTheWalls (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EyesInTheWalls = EyesInTheWalls TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesInTheWalls :: TreacheryCard EyesInTheWalls
eyesInTheWalls = treachery EyesInTheWalls Cards.eyesInTheWalls

instance RunMessage EyesInTheWalls where
  runMessage msg t@(EyesInTheWalls attrs) = case msg of
    Revelation iid source
      | isSource attrs source ->
          t <$ push (RevelationSkillTest iid source SkillWillpower (SkillTestDifficulty $ Fixed 3))
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n
      | isSource attrs source ->
          t
            <$ push (InvestigatorAssignDamage iid source DamageEvenly 0 n)
    _ -> EyesInTheWalls <$> runMessage msg attrs
