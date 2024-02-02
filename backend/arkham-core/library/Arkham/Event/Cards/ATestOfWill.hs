module Arkham.Event.Cards.ATestOfWill (
  aTestOfWill,
  ATestOfWill (..),
) where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (DrawCard)
import Arkham.Window

newtype ATestOfWill = ATestOfWill EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

aTestOfWill :: EventCard ATestOfWill
aTestOfWill = event ATestOfWill Cards.aTestOfWill

getDetails :: [Window] -> Target
getDetails ((windowType -> DrawCard who card _) : _) = BothTarget (toTarget who) (CardTarget card)
getDetails (_ : rest) = getDetails rest
getDetails [] = error "missing targets"

instance RunMessage ATestOfWill where
  runMessage msg e@(ATestOfWill attrs) = case msg of
    InvestigatorPlayEvent iid eid _ (getDetails -> details) _ | eid == toId attrs -> do
      push $ beginSkillTest iid attrs details #willpower 3
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just (BothTarget (InvestigatorTarget iid') (CardTarget c)) -> do
          canAffect <- (iid == iid' ||) <$> can.affect.otherPlayers iid
          canCancel <- c <=~> CanCancelRevelationEffect (BasicCardMatch AnyCard)
          pushWhen (canAffect && canCancel)
            $ CancelNext (toSource attrs) RevelationMessage
        _ -> error "Wrong target"
      pure e
    _ -> ATestOfWill <$> runMessage msg attrs
