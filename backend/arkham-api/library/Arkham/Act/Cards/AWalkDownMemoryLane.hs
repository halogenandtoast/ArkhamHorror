module Arkham.Act.Cards.AWalkDownMemoryLane (aWalkDownMemoryLane) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Helpers.FlavorText
import Arkham.Matcher
import Arkham.Scenarios.BadBlood.Helpers
import Arkham.Scenarios.BadBlood.Meta

newtype AWalkDownMemoryLane = AWalkDownMemoryLane ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aWalkDownMemoryLane :: ActCard AWalkDownMemoryLane
aWalkDownMemoryLane = act (1, A) AWalkDownMemoryLane Cards.aWalkDownMemoryLane Nothing

instance HasAbilities AWalkDownMemoryLane where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ memoryLocation <> LocationWithInvestigator agnesBaker)
        $ FastAbility
        $ GroupClueCost (PerPlayer 2) (LocationWithInvestigator agnesBaker)
    , mkAbility a 2 $ forced $ InvestigatorDefeated #when ByAny agnesBaker
    , restricted a 3 (notExists memoryLocation)
        $ Objective
        $ forced
        $ ScenarioEvent #after Nothing "memoryCollected"
    ]

instance RunMessage AWalkDownMemoryLane where
  runMessage msg a@(AWalkDownMemoryLane attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (memoryLocation <> LocationWithInvestigator agnesBaker)
        >>= traverse_ agnesCollectsMemoryAt
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R2
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs (attrs.ability 3)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      meta <- getBadBloodMeta
      let agnesWins = meta.agnesMemories > meta.elspethMemories
      scenarioI18n $ scope "acts.memoryOfEternalConquest" $ flavor do
        h "title"
        p.validate agnesWins "agnesWins"
        p.validate (not agnesWins) "elspethWins"
      push $ if agnesWins then R1 else R2
      pure a
    _ -> AWalkDownMemoryLane <$> liftRunMessage msg attrs
