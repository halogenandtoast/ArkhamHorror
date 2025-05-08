module Arkham.Location.Cards.HistoricalSocietyHistoricalLibrary_136 (
  historicalSocietyHistoricalLibrary_136,
) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)

newtype HistoricalSocietyHistoricalLibrary_136 = HistoricalSocietyHistoricalLibrary_136 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalLibrary_136 :: LocationCard HistoricalSocietyHistoricalLibrary_136
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
                (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (be attrs))
                (HorrorCost (toSource attrs) YouTarget 2)
              $ SkillTestResult #after You (whileInvestigating attrs) #success
          ]
        else [mkAbility attrs 1 $ forced $ EnemySpawns #when (be attrs) AnyEnemy]

instance RunMessage HistoricalSocietyHistoricalLibrary_136 where
  runMessage msg l@(HistoricalSocietyHistoricalLibrary_136 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | attrs.revealed -> do
      discoverAt NotInvestigate iid (attrs.ability 1) attrs 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      reveal attrs
      pure l
    _ -> HistoricalSocietyHistoricalLibrary_136 <$> liftRunMessage msg attrs
