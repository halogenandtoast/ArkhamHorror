module Arkham.Types.Location.Cards.HistoricalSocietyHistoricalMuseum_132
  ( historicalSocietyHistoricalMuseum_132
  , HistoricalSocietyHistoricalMuseum_132(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype HistoricalSocietyHistoricalMuseum_132 = HistoricalSocietyHistoricalMuseum_132 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalMuseum_132
  :: LocationCard HistoricalSocietyHistoricalMuseum_132
historicalSocietyHistoricalMuseum_132 = locationWithRevealedSideConnections
  HistoricalSocietyHistoricalMuseum_132
  Cards.historicalSocietyHistoricalMuseum_132
  2
  (PerPlayer 1)
  NoSymbol
  [Circle]
  Hourglass
  [Circle, Heart]

instance HasModifiersFor env HistoricalSocietyHistoricalMuseum_132 where
  getModifiersFor (SkillTestSource _ _ _ target (Just Action.Investigate)) (InvestigatorTarget _) (HistoricalSocietyHistoricalMuseum_132 attrs)
    | isTarget attrs target
    = pure $ toModifiers attrs [SkillCannotBeIncreased SkillIntellect]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HistoricalSocietyHistoricalMuseum_132 where
  getAbilities (HistoricalSocietyHistoricalMuseum_132 attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns
          Timing.When
          (LocationWithId $ toId attrs)
          AnyEnemy
      | not (locationRevealed attrs)
      ]

instance LocationRunner env => RunMessage env HistoricalSocietyHistoricalMuseum_132 where
  runMessage msg l@(HistoricalSocietyHistoricalMuseum_132 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyHistoricalMuseum_132 <$> runMessage msg attrs
