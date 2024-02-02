module Arkham.Enemy.Cards.DiscipleOfTheDevourer (
  discipleOfTheDevourer,
  DiscipleOfTheDevourer (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Sequence (AgendaStep (..), agendaStep)
import Arkham.Agenda.Types (Field (..))
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype DiscipleOfTheDevourer = DiscipleOfTheDevourer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

discipleOfTheDevourer :: EnemyCard DiscipleOfTheDevourer
discipleOfTheDevourer =
  enemyWith
    DiscipleOfTheDevourer
    Cards.discipleOfTheDevourer
    (3, Static 1, 1)
    (1, 0)
    (spawnAtL ?~ SpawnAt (FarthestLocationFromYou EmptyLocation))

instance HasAbilities DiscipleOfTheDevourer where
  getAbilities (DiscipleOfTheDevourer x) =
    withBaseAbilities
      x
      [ mkAbility x 1
          $ ForcedAbility
          $ EnemySpawns Timing.After Anywhere
          $ EnemyWithId
          $ toId x
      ]

instance RunMessage DiscipleOfTheDevourer where
  runMessage msg e@(DiscipleOfTheDevourer attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      agendaId <- selectJust AnyAgenda
      mLocationId <- field EnemyLocation (toId attrs)
      hasClues <- fieldP InvestigatorClues (> 0) iid
      step <- fieldMap AgendaSequence agendaStep agendaId
      player <- getPlayer iid
      if step == AgendaStep 1
        then
          push
            $ chooseOrRunOne player
            $ Label
              "Place 1 doom on Disciple of the Devourer"
              [PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1]
            : [ Label
                "Place one of your clues on it's location"
                [ RemoveClues (toAbilitySource attrs 1) (InvestigatorTarget iid) 1
                , PlaceClues (toAbilitySource attrs 1) (LocationTarget lid) 1
                ]
              | hasClues
              , lid <- maybeToList mLocationId
              ]
        else
          pushAll
            $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
            : case mLocationId of
              Just lid
                | hasClues ->
                    [ RemoveClues (toAbilitySource attrs 1) (InvestigatorTarget iid) 1
                    , PlaceClues (toAbilitySource attrs 1) (LocationTarget lid) 1
                    ]
              _ -> []
      pure e
    _ -> DiscipleOfTheDevourer <$> runMessage msg attrs
