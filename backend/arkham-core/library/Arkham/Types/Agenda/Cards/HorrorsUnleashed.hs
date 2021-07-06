module Arkham.Types.Agenda.Cards.HorrorsUnleashed
  ( HorrorsUnleashed(..)
  , horrorsUnleashed
  )
where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EnemyMatcher
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Resolution
import Arkham.Types.Target
import Arkham.Types.Trait

newtype HorrorsUnleashed = HorrorsUnleashed AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrorsUnleashed :: HorrorsUnleashed
horrorsUnleashed = HorrorsUnleashed
  $ baseAttrs "02239" "Horrors Unleashed" (Agenda 3 A) (Static 7)

instance HasSet Trait env EnemyId => HasModifiersFor env HorrorsUnleashed where
  getModifiersFor _ (EnemyTarget eid) (HorrorsUnleashed attrs) = do
    isAbomination <- member Abomination <$> getSet eid
    if isAbomination
      then pure $ toModifiers attrs [EnemyFight 1, EnemyEvade 1]
      else pure []
  getModifiersFor _ _ _ = pure []

instance HasActions env HorrorsUnleashed where
  getActions i window (HorrorsUnleashed x) = getActions i window x

instance AgendaRunner env => RunMessage env HorrorsUnleashed where
  runMessage msg a@(HorrorsUnleashed attrs) = case msg of
    EndEnemy -> do
      leadInvestigatorId <- getLeadInvestigatorId
      broodOfYogSothoth <- map EnemyTarget
        <$> getSetList (EnemyWithTitle "Brood of Yog-Sothoth")
      a <$ push
        (chooseOneAtATime
          leadInvestigatorId
          [ TargetLabel target [ChooseRandomLocation target mempty]
          | target <- broodOfYogSothoth
          ]
        )
    ChosenRandomLocation target@(EnemyTarget _) lid ->
      a <$ push (MoveToward target (LocationWithId lid))
    AdvanceAgenda aid | aid == agendaId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    _ -> HorrorsUnleashed <$> runMessage msg attrs
