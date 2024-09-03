module Arkham.Location.Cards.HistoricalSocietyMeetingRoom (
  historicalSocietyMeetingRoom,
  HistoricalSocietyMeetingRoom (..),
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (DiscoverClues, RevealLocation)
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Trait

newtype HistoricalSocietyMeetingRoom = HistoricalSocietyMeetingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyMeetingRoom :: LocationCard HistoricalSocietyMeetingRoom
historicalSocietyMeetingRoom =
  location
    HistoricalSocietyMeetingRoom
    Cards.historicalSocietyMeetingRoom
    4
    (PerPlayer 1)

instance HasAbilities HistoricalSocietyMeetingRoom where
  getAbilities (HistoricalSocietyMeetingRoom attrs) =
    extend attrs
      $ if attrs.revealed
        then
          [ restrictedAbility
              attrs
              1
              (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (LocationWithId attrs.id))
              $ actionAbilityWithCost (ExhaustAssetCost $ AssetWithTrait Ally)
          ]
        else
          [mkAbility attrs 1 $ forced $ EnemySpawns #when (LocationWithId attrs.id) AnyEnemy]

instance RunMessage HistoricalSocietyMeetingRoom where
  runMessage msg l@(HistoricalSocietyMeetingRoom attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | attrs.revealed -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 1) 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Msg.RevealLocation Nothing attrs.id
      pure l
    _ -> HistoricalSocietyMeetingRoom <$> runMessage msg attrs
