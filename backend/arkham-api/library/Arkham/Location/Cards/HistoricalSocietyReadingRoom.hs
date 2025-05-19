module Arkham.Location.Cards.HistoricalSocietyReadingRoom (historicalSocietyReadingRoom) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.EchoesOfThePast.Helpers

newtype HistoricalSocietyReadingRoom = HistoricalSocietyReadingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyReadingRoom :: LocationCard HistoricalSocietyReadingRoom
historicalSocietyReadingRoom = location HistoricalSocietyReadingRoom Cards.historicalSocietyReadingRoom 5 (Static 1)

instance HasAbilities HistoricalSocietyReadingRoom where
  getAbilities (HistoricalSocietyReadingRoom attrs) =
    withBaseAbilities attrs
      $ if attrs.revealed
        then
          [ scenarioI18n
              $ withI18nTooltip "historicalSocietyReadingRoom.investigate"
              $ groupLimit PerRound
              $ investigateAbility attrs 1 mempty Here
          ]
        else [mkAbility attrs 1 $ ForcedAbility $ EnemySpawns #when (LocationWithId $ toId attrs) AnyEnemy]

instance RunMessage HistoricalSocietyReadingRoom where
  runMessage msg l@(HistoricalSocietyReadingRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | locationRevealed attrs -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 | not (locationRevealed attrs) -> do
      reveal attrs
      pure l
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      enemies <- select $ EnemyWithDoom $ atLeast 1
      chooseOrRunOneM iid do
        targets enemies \enemy -> do
          removeDoom (attrs.ability 1) enemy 1
          placeClues (attrs.ability 1) iid 1
      pure l
    _ -> HistoricalSocietyReadingRoom <$> liftRunMessage msg attrs
