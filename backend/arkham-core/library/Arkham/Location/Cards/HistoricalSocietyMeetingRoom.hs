module Arkham.Location.Cards.HistoricalSocietyMeetingRoom
  ( historicalSocietyMeetingRoom
  , HistoricalSocietyMeetingRoom(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher hiding (DiscoverClues, RevealLocation)
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype HistoricalSocietyMeetingRoom = HistoricalSocietyMeetingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyMeetingRoom :: LocationCard HistoricalSocietyMeetingRoom
historicalSocietyMeetingRoom = locationWith
  HistoricalSocietyMeetingRoom
  Cards.historicalSocietyMeetingRoom
  4
  (PerPlayer 1)
  NoSymbol
  [Square]
  (revealedSymbolL .~ Diamond)

instance HasAbilities HistoricalSocietyMeetingRoom where
  getAbilities (HistoricalSocietyMeetingRoom attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility
            attrs
            1
            (Here <> CluesOnThis (AtLeast $ Static 1) <> CanDiscoverClues)
          $ ActionAbility Nothing
          $ Costs [ActionCost 1, ExhaustAssetCost $ AssetWithTrait Ally]
        ]
      else
        [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns
            Timing.When
            (LocationWithId $ toId attrs)
            AnyEnemy
        ]

instance LocationRunner env => RunMessage env HistoricalSocietyMeetingRoom where
  runMessage msg l@(HistoricalSocietyMeetingRoom attrs) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l
      <$ push (DiscoverClues iid (toId attrs) 1 Nothing)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyMeetingRoom <$> runMessage msg attrs
