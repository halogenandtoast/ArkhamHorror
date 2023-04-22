module Arkham.Location.Cards.HistoricalSocietyHistoricalLibrary_136
  ( historicalSocietyHistoricalLibrary_136
  , HistoricalSocietyHistoricalLibrary_136(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message qualified as Msg
import Arkham.Timing qualified as Timing

newtype HistoricalSocietyHistoricalLibrary_136 = HistoricalSocietyHistoricalLibrary_136 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalLibrary_136
  :: LocationCard HistoricalSocietyHistoricalLibrary_136
historicalSocietyHistoricalLibrary_136 = location
  HistoricalSocietyHistoricalLibrary_136
  Cards.historicalSocietyHistoricalLibrary_136
  3
  (PerPlayer 2)

instance HasAbilities HistoricalSocietyHistoricalLibrary_136 where
  getAbilities (HistoricalSocietyHistoricalLibrary_136 attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility
            attrs
            1
            (Here <> CluesOnThis (AtLeast $ Static 1) <> CanDiscoverCluesAt
              (LocationWithId $ toId attrs)
            )
            (ReactionAbility
              (SkillTestResult
                Timing.After
                You
                (WhileInvestigating $ LocationWithId $ toId attrs)
                (SuccessResult AnyValue)
              )
              (HorrorCost (toSource attrs) YouTarget 2)
            )
          & abilityLimitL
          .~ PlayerLimit PerRound 1
        ]
      else
        [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns
            Timing.When
            (LocationWithId $ toId attrs)
            AnyEnemy
        ]

instance RunMessage HistoricalSocietyHistoricalLibrary_136 where
  runMessage msg l@(HistoricalSocietyHistoricalLibrary_136 attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source && locationRevealed attrs -> l
      <$ push (InvestigatorDiscoverClues iid (toId attrs) 1 Nothing)
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      l <$ push (Msg.RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyHistoricalLibrary_136 <$> runMessage msg attrs
