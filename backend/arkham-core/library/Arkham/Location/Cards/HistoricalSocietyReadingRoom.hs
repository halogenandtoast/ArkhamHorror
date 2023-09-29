module Arkham.Location.Cards.HistoricalSocietyReadingRoom (
  historicalSocietyReadingRoom,
  HistoricalSocietyReadingRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigate
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg

newtype HistoricalSocietyReadingRoom = HistoricalSocietyReadingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyReadingRoom :: LocationCard HistoricalSocietyReadingRoom
historicalSocietyReadingRoom = location HistoricalSocietyReadingRoom Cards.historicalSocietyReadingRoom 5 (Static 1)

instance HasAbilities HistoricalSocietyReadingRoom where
  getAbilities (HistoricalSocietyReadingRoom attrs) =
    withBaseAbilities attrs
      $ if locationRevealed attrs
        then
          [ withTooltip
              "{action}: _Investigate_. If you succeed, instead of discovering clues, choose an enemy with doom on it. Take 1 of that enemy's doom, flip it to its clue side, and place it on your investigator. (Group limit once per round)."
              $ groupLimit PerRound
              $ investigateAbility attrs 1 mempty Here
          ]
        else [mkAbility attrs 1 $ ForcedAbility $ EnemySpawns #when (LocationWithId $ toId attrs) AnyEnemy]

instance RunMessage HistoricalSocietyReadingRoom where
  runMessage msg l@(HistoricalSocietyReadingRoom attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | locationRevealed attrs -> do
      pushM $ mkInvestigate iid (toAbilitySource attrs 1)
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 | not (locationRevealed attrs) -> do
      push $ Msg.RevealLocation Nothing (toId attrs)
      pure l
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      enemies <- selectListMap EnemyTarget $ EnemyWithDoom $ atLeast 1
      pushIfAny enemies
        $ chooseOrRunOne iid
        $ [ targetLabel
            target
            [ RemoveDoom (toAbilitySource attrs 1) target 1
            , PlaceClues (toAbilitySource attrs 1) (InvestigatorTarget iid) 1
            ]
          | target <- enemies
          ]
      pure l
    _ -> HistoricalSocietyReadingRoom <$> runMessage msg attrs
