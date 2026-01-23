module Arkham.Location.Cards.MineralTunnel (mineralTunnel) where

import Arkham.Difficulty
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MineralTunnel = MineralTunnel LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mineralTunnel :: LocationCard MineralTunnel
mineralTunnel = locationWith MineralTunnel Cards.mineralTunnel 0 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor MineralTunnel where
  getModifiersFor (MineralTunnel a) = modifySelfMaybe a do
    liftGuardM $ getIsBeingInvestigated a
    iid <- MaybeT getSkillTestInvestigator
    n <- lift $ selectCount $ inHandOf NotForPlay iid
    guard (n > 0)
    difficulty <- lift getDifficulty
    pure [ShroudModifier $ if difficulty `elem` [Easy, Standard] then min 6 n else n]
