module Arkham.Types.Location.Cards.HistoricalSocietyMeetingRoom
  ( historicalSocietyMeetingRoom
  , HistoricalSocietyMeetingRoom(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher hiding (DiscoverClues, RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait

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
