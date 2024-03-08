module Arkham.Location.Cards.HistoricalSocietyHistoricalLibrary_133 (
  historicalSocietyHistoricalLibrary_133,
  HistoricalSocietyHistoricalLibrary_133 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg
import Arkham.Timing qualified as Timing

newtype HistoricalSocietyHistoricalLibrary_133 = HistoricalSocietyHistoricalLibrary_133 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalLibrary_133
  :: LocationCard HistoricalSocietyHistoricalLibrary_133
historicalSocietyHistoricalLibrary_133 =
  location
    HistoricalSocietyHistoricalLibrary_133
    Cards.historicalSocietyHistoricalLibrary_133
    3
    (PerPlayer 2)

instance HasAbilities HistoricalSocietyHistoricalLibrary_133 where
  getAbilities (HistoricalSocietyHistoricalLibrary_133 attrs) =
    withBaseAbilities attrs
      $ if locationRevealed attrs
        then
          [ limitedAbility (PlayerLimit PerRound 1)
              $ restrictedAbility
                attrs
                1
                ( Here
                    <> CluesOnThis (AtLeast $ Static 1)
                    <> CanDiscoverCluesAt
                      (LocationWithId $ toId attrs)
                )
              $ ReactionAbility
                ( SkillTestResult
                    Timing.After
                    You
                    (WhileInvestigating $ LocationWithId $ toId attrs)
                    (SuccessResult AnyValue)
                )
                (HorrorCost (toSource attrs) YouTarget 2)
          ]
        else
          [ mkAbility attrs 1
              $ ForcedAbility
              $ EnemySpawns
                Timing.When
                (LocationWithId $ toId attrs)
                AnyEnemy
          ]

instance RunMessage HistoricalSocietyHistoricalLibrary_133 where
  runMessage msg l@(HistoricalSocietyHistoricalLibrary_133 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ | locationRevealed attrs -> do
      push (InvestigatorDiscoverClues iid (toId attrs) (toAbilitySource attrs 1) 1 Nothing)
      pure l
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push (Msg.RevealLocation Nothing $ toId attrs)
      pure l
    _ -> HistoricalSocietyHistoricalLibrary_133 <$> runMessage msg attrs
