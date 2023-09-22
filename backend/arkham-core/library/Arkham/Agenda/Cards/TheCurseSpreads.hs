module Arkham.Agenda.Cards.TheCurseSpreads (
  TheCurseSpreads (..),
  theCurseSpreads,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message

newtype TheCurseSpreads = TheCurseSpreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCurseSpreads :: AgendaCard TheCurseSpreads
theCurseSpreads = agenda (3, A) TheCurseSpreads Cards.theCurseSpreads (Static 8)

instance HasAbilities TheCurseSpreads where
  getAbilities (TheCurseSpreads x) = [mkAbility x 1 $ ForcedAbility $ PhaseEnds #when #investigation]

instance RunMessage TheCurseSpreads where
  runMessage msg a@(TheCurseSpreads attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      notEngaged <- selectAny $ UnengagedEnemy <> enemyIs Cards.theRougarou
      pushWhen notEngaged $ placeDoom (toAbilitySource attrs 1) attrs 1
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push R1
      pure a
    _ -> TheCurseSpreads <$> runMessage msg attrs
