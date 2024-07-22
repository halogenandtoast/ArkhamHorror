module Arkham.Event.Cards.ATestOfWill (aTestOfWill, ATestOfWill (..)) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Target (getSkillTestTarget)
import Arkham.Matcher hiding (DrawCard)
import Arkham.Window

newtype ATestOfWill = ATestOfWill EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTestOfWill :: EventCard ATestOfWill
aTestOfWill = event ATestOfWill Cards.aTestOfWill

getDetails :: [Window] -> Target
getDetails ((windowType -> DrawCard who card _) : _) = BothTarget (toTarget who) (CardTarget card)
getDetails (_ : rest) = getDetails rest
getDetails [] = error "missing targets"

instance RunMessage ATestOfWill where
  runMessage msg e@(ATestOfWill attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ (getDetails -> details) _ | eid == toId attrs -> do
      sid <- getRandom
      beginSkillTest sid iid attrs details #willpower (Fixed 3)
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTarget >>= \case
        Just (BothTarget (InvestigatorTarget iid') (CardTarget c)) -> do
          canAffect <- (iid == iid' ||) <$> can.affect.otherPlayers iid
          canCancel <- c <=~> CanCancelRevelationEffect (basic AnyCard)
          when (canAffect && canCancel) $ cancelRevelation attrs c
        _ -> error "Wrong target"
      pure e
    _ -> ATestOfWill <$> liftRunMessage msg attrs
