module Arkham.Location.Cards.HistoricalSocietyHistoricalLibrary_136 (
  historicalSocietyHistoricalLibrary_136,
  HistoricalSocietyHistoricalLibrary_136 (..),
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype HistoricalSocietyHistoricalLibrary_136 = HistoricalSocietyHistoricalLibrary_136 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalLibrary_136
  :: LocationCard HistoricalSocietyHistoricalLibrary_136
historicalSocietyHistoricalLibrary_136 =
  location
    HistoricalSocietyHistoricalLibrary_136
    Cards.historicalSocietyHistoricalLibrary_136
    3
    (PerPlayer 2)

instance HasAbilities HistoricalSocietyHistoricalLibrary_136 where
  getAbilities (HistoricalSocietyHistoricalLibrary_136 attrs) =
    extend attrs
      $ if attrs.revealed
        then
          [ playerLimit PerRound
              $ reaction
                attrs
                1
                (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (LocationWithId attrs.id))
                (HorrorCost (toSource attrs) YouTarget 2)
              $ SkillTestResult #after You (whileInvestigating attrs) (SuccessResult AnyValue)
          ]
        else
          [mkAbility attrs 1 $ forced $ EnemySpawns #when (LocationWithId attrs.id) AnyEnemy]

instance RunMessage HistoricalSocietyHistoricalLibrary_136 where
  runMessage msg l@(HistoricalSocietyHistoricalLibrary_136 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | attrs.revealed -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 1) 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Msg.RevealLocation Nothing attrs.id
      pure l
    _ -> HistoricalSocietyHistoricalLibrary_136 <$> runMessage msg attrs
