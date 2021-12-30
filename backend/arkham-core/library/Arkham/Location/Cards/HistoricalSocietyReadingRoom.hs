module Arkham.Location.Cards.HistoricalSocietyReadingRoom
  ( historicalSocietyReadingRoom
  , HistoricalSocietyReadingRoom(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Location.Helpers
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype HistoricalSocietyReadingRoom = HistoricalSocietyReadingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyReadingRoom :: LocationCard HistoricalSocietyReadingRoom
historicalSocietyReadingRoom = locationWithRevealedSideConnections
  HistoricalSocietyReadingRoom
  Cards.historicalSocietyReadingRoom
  5
  (Static 1)
  NoSymbol
  [Circle]
  T
  [Circle]

instance HasAbilities HistoricalSocietyReadingRoom where
  getAbilities (HistoricalSocietyReadingRoom attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility
            attrs
            1
            Here
            (ActionAbility (Just Action.Investigate) (ActionCost 1))
          & abilityLimitL
          .~ GroupLimit PerRound 1
        ]
      else
        [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns
            Timing.When
            (LocationWithId $ toId attrs)
            AnyEnemy
        ]

instance LocationRunner env => RunMessage env HistoricalSocietyReadingRoom where
  runMessage msg l@(HistoricalSocietyReadingRoom attrs) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l <$ push
        (Investigate
          iid
          (toId attrs)
          (AbilitySource source 1)
          Nothing
          SkillIntellect
          False
        )
    UseCardAbility _ source _ 1 _
      | isSource attrs source && not (locationRevealed attrs) -> l
      <$ push (RevealLocation Nothing $ toId attrs)
    Successful (Action.Investigate, _) iid (AbilitySource source 1) _ _
      | isSource attrs source -> do
        enemies <- selectListMap EnemyTarget $ EnemyWithDoom $ AtLeast $ Static
          1
        l <$ when
          (notNull enemies)
          (push $ chooseOrRunOne
            iid
            [ TargetLabel
                target
                [RemoveDoom target 1, PlaceClues (InvestigatorTarget iid) 1]
            | target <- enemies
            ]
          )
    _ -> HistoricalSocietyReadingRoom <$> runMessage msg attrs
