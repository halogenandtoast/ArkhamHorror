module Arkham.Agenda.Cards.HorrorsUnleashed (
  HorrorsUnleashed (..),
  horrorsUnleashed,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Matcher hiding (ChosenRandomLocation)
import Arkham.Modifier qualified as Modifier
import Arkham.Projection
import Arkham.Trait

newtype HorrorsUnleashed = HorrorsUnleashed AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

horrorsUnleashed :: AgendaCard HorrorsUnleashed
horrorsUnleashed = agenda (3, A) HorrorsUnleashed Cards.horrorsUnleashed (Static 7)

instance HasAbilities HorrorsUnleashed where
  getAbilities (HorrorsUnleashed x) = [mkAbility x 1 $ ForcedAbility $ PhaseEnds #when #enemy]

instance HasModifiersFor HorrorsUnleashed where
  getModifiersFor (EnemyTarget eid) (HorrorsUnleashed attrs) = do
    isAbomination <- member Abomination <$> field EnemyTraits eid
    pure $ toModifiers attrs $ guard isAbomination *> [Modifier.EnemyFight 1, Modifier.EnemyEvade 1]
  getModifiersFor _ _ = pure []

instance RunMessage HorrorsUnleashed where
  runMessage msg a@(HorrorsUnleashed attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      broodOfYogSothoth <- selectTargets $ EnemyWithTitle "Brood of Yog-Sothoth"
      pushWhen (notNull broodOfYogSothoth)
        $ chooseOneAtATime lead
        $ targetLabels broodOfYogSothoth (\target -> only $ ChooseRandomLocation target mempty)
      pure a
    ChosenRandomLocation target@(EnemyTarget _) lid -> do
      push $ MoveToward target (LocationWithId lid)
      pure a
    AdvanceAgenda aid | aid == agendaId attrs && onSide B attrs -> do
      push R1
      pure a
    _ -> HorrorsUnleashed <$> runMessage msg attrs
