module Arkham.Location.Cards.SacredWoods_185 (
  sacredWoods_185,
  SacredWoods_185 (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype SacredWoods_185 = SacredWoods_185 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sacredWoods_185 :: LocationCard SacredWoods_185
sacredWoods_185 = locationWith SacredWoods_185 Cards.sacredWoods_185 6 (PerPlayer 1) (labelL .~ "star")

instance HasModifiersFor SacredWoods_185 where
  getModifiersFor (InvestigatorTarget iid) (SacredWoods_185 a) = do
    here <- iid `isAt` a
    pure $ toModifiers a [IncreaseCostOf AnyCard 2 | here]
  getModifiersFor target (SacredWoods_185 a) | isTarget a target = do
    miid <- getSkillTestInvestigator
    case miid of
      Nothing -> pure []
      Just iid -> do
        n <- selectCount $ assetControlledBy iid
        isBeingInvestigated <- getIsBeingInvestigated (toId a)
        pure $ toModifiers a [ShroudModifier (-n) | isBeingInvestigated]
  getModifiersFor _ _ = pure []

instance RunMessage SacredWoods_185 where
  runMessage msg (SacredWoods_185 attrs) = SacredWoods_185 <$> runMessage msg attrs
