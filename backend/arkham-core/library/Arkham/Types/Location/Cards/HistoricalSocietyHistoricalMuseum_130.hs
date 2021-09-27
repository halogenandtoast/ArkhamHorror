module Arkham.Types.Location.Cards.HistoricalSocietyHistoricalMuseum_130
  ( historicalSocietyHistoricalMuseum_130
  , HistoricalSocietyHistoricalMuseum_130(..)
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

newtype HistoricalSocietyHistoricalMuseum_130 = HistoricalSocietyHistoricalMuseum_130 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalMuseum_130
  :: LocationCard HistoricalSocietyHistoricalMuseum_130
historicalSocietyHistoricalMuseum_130 = locationWithRevealedSideConnections
  HistoricalSocietyHistoricalMuseum_130
  Cards.historicalSocietyHistoricalMuseum_130
  2
  (PerPlayer 1)
  NoSymbol
  [Square]
  Heart
  [Square, Hourglass]

instance HasModifiersFor env HistoricalSocietyHistoricalMuseum_130 where
  getModifiersFor (SkillTestSource _ _ _ target (Just Action.Investigate)) (InvestigatorTarget _) (HistoricalSocietyHistoricalMuseum_130 attrs)
    | isTarget attrs target
    = pure $ toModifiers attrs [SkillCannotBeIncreased SkillIntellect]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HistoricalSocietyHistoricalMuseum_130 where
  getAbilities (HistoricalSocietyHistoricalMuseum_130 attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns
          Timing.When
          (LocationWithId $ toId attrs)
          AnyEnemy
      | not (locationRevealed attrs)
      ]

instance LocationRunner env => RunMessage env HistoricalSocietyHistoricalMuseum_130 where
  runMessage msg l@(HistoricalSocietyHistoricalMuseum_130 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyHistoricalMuseum_130 <$> runMessage msg attrs
