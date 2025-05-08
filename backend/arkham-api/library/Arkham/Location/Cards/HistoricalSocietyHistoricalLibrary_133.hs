module Arkham.Location.Cards.HistoricalSocietyHistoricalLibrary_133 (
  historicalSocietyHistoricalLibrary_133,
) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)

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
  getAbilities (HistoricalSocietyHistoricalLibrary_133 a) =
    withBaseAbilities a
      $ if a.revealed
        then
          [ playerLimit PerRound
              $ reaction
                a
                1
                (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (be a))
                (HorrorCost (toSource a) YouTarget 2)
              $ SkillTestResult #after You (whileInvestigating a) #success
          ]
        else [mkAbility a 1 $ forced $ EnemySpawns #when (be a) AnyEnemy]

instance RunMessage HistoricalSocietyHistoricalLibrary_133 where
  runMessage msg l@(HistoricalSocietyHistoricalLibrary_133 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | attrs.revealed -> do
      discoverAt NotInvestigate iid (attrs.ability 1) attrs 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      reveal attrs
      pure l
    _ -> HistoricalSocietyHistoricalLibrary_133 <$> liftRunMessage msg attrs
