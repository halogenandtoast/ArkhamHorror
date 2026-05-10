module Arkham.Location.Cards.LairOfDagon (lairOfDagon) where

import Arkham.Ability
import Arkham.Helpers.Investigator
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheLairOfDagon.Helpers

newtype LairOfDagon = LairOfDagon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lairOfDagon :: LocationCard LairOfDagon
lairOfDagon = location LairOfDagon Cards.lairOfDagon 3 (PerPlayer 3)

instance HasAbilities LairOfDagon where
  getAbilities (LairOfDagon a) =
    extendRevealed
      a
      [ forcedAbility a 1 $ Enters #after You $ be a
      , skillTestAbility $ restricted a 2 Here actionAbility
      ]

instance RunMessage LairOfDagon where
  runMessage msg l@(LairOfDagon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      choices <- mins <$> traverse (traverseToSnd (`getSkillValue` iid)) [minBound .. maxBound]
      sid <- getRandom
      chooseOrRunOneM iid do
        for_ choices \skill -> do
          skillLabeled skill $ beginSkillTest sid iid (attrs.ability 1) iid skill (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOrRunOneM iid do
        for_ [#willpower, #agility] \skill -> do
          skillLabeled skill $ beginSkillTest sid iid (attrs.ability 2) iid skill (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      clues <- iid.clues
      when (clues > 0) do
        act <- selectJust AnyAct
        moveTokens (attrs.ability 2) iid act #clue 1
        ks <- iid.keys
        when (notNull ks && clues > 1) do
          chooseOneM iid $ scenarioI18n $ scope "lairOfDagon" do
            labeled' "doNotSpendKey" nothing
            for_ ks \k -> do
              keyVar "key" (keyName k) $ labeled' "spendKey" do
                placeKey ScenarioTarget k
                moveTokens (attrs.ability 2) iid act #clue 1
      pure l
    _ -> LairOfDagon <$> liftRunMessage msg attrs
