module Arkham.Location.Cards.HistoricalSocietyHistoricalLibrary_133 (
  historicalSocietyHistoricalLibrary_133,
  HistoricalSocietyHistoricalLibrary_133 (..),
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

instance RunMessage HistoricalSocietyHistoricalLibrary_133 where
  runMessage msg l@(HistoricalSocietyHistoricalLibrary_133 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | attrs.revealed -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 1) 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Msg.RevealLocation Nothing attrs.id
      pure l
    _ -> HistoricalSocietyHistoricalLibrary_133 <$> runMessage msg attrs
