module Arkham.Location.Cards.HistoricalSocietyMeetingRoom (
  historicalSocietyMeetingRoom,
  HistoricalSocietyMeetingRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (DiscoverClues, RevealLocation)
import Arkham.Message qualified as Msg
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype HistoricalSocietyMeetingRoom = HistoricalSocietyMeetingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

historicalSocietyMeetingRoom :: LocationCard HistoricalSocietyMeetingRoom
historicalSocietyMeetingRoom =
  location
    HistoricalSocietyMeetingRoom
    Cards.historicalSocietyMeetingRoom
    4
    (PerPlayer 1)

instance HasAbilities HistoricalSocietyMeetingRoom where
  getAbilities (HistoricalSocietyMeetingRoom attrs) =
    withBaseAbilities attrs
      $ if locationRevealed attrs
        then
          [ restrictedAbility
              attrs
              1
              ( Here
                  <> CluesOnThis (AtLeast $ Static 1)
                  <> CanDiscoverCluesAt
                    (LocationWithId $ toId attrs)
              )
              $ ActionAbility []
              $ Costs [ActionCost 1, ExhaustAssetCost $ AssetWithTrait Ally]
          ]
        else
          [ mkAbility attrs 1
              $ ForcedAbility
              $ EnemySpawns
                Timing.When
                (LocationWithId $ toId attrs)
                AnyEnemy
          ]

instance RunMessage HistoricalSocietyMeetingRoom where
  runMessage msg l@(HistoricalSocietyMeetingRoom attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ | locationRevealed attrs -> do
      push $ InvestigatorDiscoverClues iid (toId attrs) (toAbilitySource attrs 1) 1 Nothing
      pure l
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ Msg.RevealLocation Nothing $ toId attrs
      pure l
    _ -> HistoricalSocietyMeetingRoom <$> runMessage msg attrs
