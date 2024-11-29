module Arkham.Location.Cards.HistoricalSocietyHistoricalMuseum_130 (
  historicalSocietyHistoricalMuseum_130,
  HistoricalSocietyHistoricalMuseum_130 (..),
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype HistoricalSocietyHistoricalMuseum_130 = HistoricalSocietyHistoricalMuseum_130 LocationAttrs
  deriving anyclass IsLocation
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
    investigating <- isInvestigating iid a.id
    toModifiers a [SkillCannotBeIncreased #intellect | investigating]
  getModifiersFor _ _ = pure []

instance HasAbilities HistoricalSocietyHistoricalMuseum_130 where
  getAbilities (HistoricalSocietyHistoricalMuseum_130 attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
        $ forced
        $ EnemySpawns #when (be attrs) AnyEnemy
      | not (locationRevealed attrs)
      ]

instance RunMessage HistoricalSocietyHistoricalMuseum_130 where
  runMessage msg l@(HistoricalSocietyHistoricalMuseum_130 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Msg.RevealLocation Nothing attrs.id
      pure l
    _ -> HistoricalSocietyHistoricalMuseum_130 <$> runMessage msg attrs
