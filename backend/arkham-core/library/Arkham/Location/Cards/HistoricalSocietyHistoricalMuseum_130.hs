module Arkham.Location.Cards.HistoricalSocietyHistoricalMuseum_130 (
  historicalSocietyHistoricalMuseum_130,
  HistoricalSocietyHistoricalMuseum_130 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype HistoricalSocietyHistoricalMuseum_130 = HistoricalSocietyHistoricalMuseum_130 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalMuseum_130
  :: LocationCard HistoricalSocietyHistoricalMuseum_130
historicalSocietyHistoricalMuseum_130 =
  location
    HistoricalSocietyHistoricalMuseum_130
    Cards.historicalSocietyHistoricalMuseum_130
    2
    (PerPlayer 1)

instance HasModifiersFor HistoricalSocietyHistoricalMuseum_130 where
  getModifiersFor (InvestigatorTarget iid) (HistoricalSocietyHistoricalMuseum_130 a) = do
    investigating <- isInvestigating iid (toId a)
    pure $ toModifiers a [SkillCannotBeIncreased SkillIntellect | investigating]
  getModifiersFor _ _ = pure []

instance HasAbilities HistoricalSocietyHistoricalMuseum_130 where
  getAbilities (HistoricalSocietyHistoricalMuseum_130 attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $
        ForcedAbility $
          EnemySpawns
            Timing.When
            (LocationWithId $ toId attrs)
            AnyEnemy
      | not (locationRevealed attrs)
      ]

instance RunMessage HistoricalSocietyHistoricalMuseum_130 where
  runMessage msg l@(HistoricalSocietyHistoricalMuseum_130 attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          l <$ push (Msg.RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyHistoricalMuseum_130 <$> runMessage msg attrs
