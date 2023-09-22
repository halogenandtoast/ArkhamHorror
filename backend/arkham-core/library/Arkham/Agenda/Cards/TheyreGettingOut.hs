module Arkham.Agenda.Cards.TheyreGettingOut (
  TheyreGettingOut (..),
  theyreGettingOut,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Trait

newtype TheyreGettingOut = TheyreGettingOut AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyreGettingOut :: AgendaCard TheyreGettingOut
theyreGettingOut = agenda (3, A) TheyreGettingOut Cards.theyreGettingOut (Static 10)

instance HasAbilities TheyreGettingOut where
  getAbilities (TheyreGettingOut x) =
    [ forcedAbility x 1 $ PhaseEnds #when #enemy
    , forcedAbility x 2 (RoundEnds #when)
        `withCriteria` enemyExists
          (UnengagedEnemy <> EnemyWithTrait Ghoul <> NotEnemy (EnemyAt "Parlor"))
    ]

instance RunMessage TheyreGettingOut where
  runMessage msg a@(TheyreGettingOut attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      actSequence <- fieldMap ActSequence (AS.unActStep . AS.actStep) =<< selectJust AnyAct
      let resolution = if actSequence `elem` [1, 2] then Resolution 3 else NoResolution
      push $ ScenarioResolution resolution
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      lead <- getLead
      enemiesToMove <- selectList $ UnengagedEnemy <> EnemyWithTrait Ghoul <> NotEnemy (EnemyAt "Parlor")

      pushWhen (notNull enemiesToMove)
        $ chooseOneAtATime lead
        $ targetLabels enemiesToMove (\enemy -> only $ MoveToward (toTarget enemy) "Parlor")
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      ghoulCount <- selectCount $ EnemyWithTrait Ghoul <> EnemyAt (LocationMatchAny ["Parlor", "Hallway"])
      pushAll $ replicate ghoulCount PlaceDoomOnAgenda
      pure a
    _ -> TheyreGettingOut <$> runMessage msg attrs
