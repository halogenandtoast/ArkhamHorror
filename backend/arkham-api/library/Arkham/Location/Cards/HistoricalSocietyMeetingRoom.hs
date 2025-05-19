module Arkham.Location.Cards.HistoricalSocietyMeetingRoom (historicalSocietyMeetingRoom) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DiscoverClues, RevealLocation)
import Arkham.Trait

newtype HistoricalSocietyMeetingRoom = HistoricalSocietyMeetingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyMeetingRoom :: LocationCard HistoricalSocietyMeetingRoom
historicalSocietyMeetingRoom =
  location HistoricalSocietyMeetingRoom Cards.historicalSocietyMeetingRoom 4 (PerPlayer 1)

instance HasAbilities HistoricalSocietyMeetingRoom where
  getAbilities (HistoricalSocietyMeetingRoom a)
    | a.revealed =
        extend1 a
          $ restricted a 1 (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (be a))
          $ actionAbilityWithCost (ExhaustAssetCost $ AssetWithTrait Ally)
  getAbilities (HistoricalSocietyMeetingRoom a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemySpawns #when (be a) AnyEnemy

instance RunMessage HistoricalSocietyMeetingRoom where
  runMessage msg l@(HistoricalSocietyMeetingRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | attrs.revealed -> do
      discoverAt NotInvestigate iid (attrs.ability 1) attrs 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      reveal attrs
      pure l
    _ -> HistoricalSocietyMeetingRoom <$> liftRunMessage msg attrs
