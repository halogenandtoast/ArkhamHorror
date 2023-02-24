module Arkham.Location.Cards.HistoricalSocietyReadingRoom
  ( historicalSocietyReadingRoom
  , HistoricalSocietyReadingRoom(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Timing qualified as Timing

newtype HistoricalSocietyReadingRoom = HistoricalSocietyReadingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyReadingRoom :: LocationCard HistoricalSocietyReadingRoom
historicalSocietyReadingRoom = location
  HistoricalSocietyReadingRoom
  Cards.historicalSocietyReadingRoom
  5
  (Static 1)

instance HasAbilities HistoricalSocietyReadingRoom where
  getAbilities (HistoricalSocietyReadingRoom attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ withTooltip
            "{action}: _Investigate_. If you succeed, instead of discovering clues, choose an enemy with doom on it. Take 1 of that enemy's doom, flip it to its clue side, and place it on your investigator. (Group limit once per round)."
          $ limitedAbility (GroupLimit PerRound 1)
          $ restrictedAbility
              attrs
              1
              Here
              (ActionAbility (Just Action.Investigate) (ActionCost 1))
        ]
      else
        [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns
            Timing.When
            (LocationWithId $ toId attrs)
            AnyEnemy
        ]

instance RunMessage HistoricalSocietyReadingRoom where
  runMessage msg l@(HistoricalSocietyReadingRoom attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source && locationRevealed attrs -> l <$ push
        (Investigate
          iid
          (toId attrs)
          (AbilitySource source 1)
          Nothing
          SkillIntellect
          False
        )
    UseCardAbility _ source 1 _ _
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
