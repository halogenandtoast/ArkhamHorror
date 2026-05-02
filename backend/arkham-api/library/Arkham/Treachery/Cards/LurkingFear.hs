module Arkham.Treachery.Cards.LurkingFear (lurkingFear) where

import Arkham.Helpers.Enemy
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Spawn
import Arkham.Trait (Trait (Dark))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LurkingFear = LurkingFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lurkingFear :: TreacheryCard LurkingFear
lurkingFear = treachery LurkingFear Cards.lurkingFear

instance RunMessage LurkingFear where
  runMessage msg t@(LurkingFear attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility
        $ IfLocationExistsCalculation
          (locationWithInvestigator iid <> LocationWithTrait Dark)
          (Fixed 5)
          (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      enemies <- pursuitEnemiesWithHighestEvade
      chooseOrRunOneM iid $ scenarioI18n do
        unscoped $ countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
        labeledValidate' (notNull enemies) "lurkingFear.pursuit" do
          chooseTargetM iid enemies \e -> spawnAt e Nothing (SpawnEngagedWith $ InvestigatorWithId iid)
      pure t
    _ -> LurkingFear <$> liftRunMessage msg attrs
