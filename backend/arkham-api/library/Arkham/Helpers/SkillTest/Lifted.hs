module Arkham.Helpers.SkillTest.Lifted (module Arkham.Helpers.SkillTest.Lifted, module X) where

import Arkham.Calculation
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message (toMessage)
import Arkham.Helpers.Ref (sourceToTarget)
import Arkham.Id
import Arkham.Investigate (mkInvestigateLocation)
import Arkham.Investigate.Types qualified as I
import Arkham.Message (Message (..))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.SkillTest.Base (SkillTest (skillTestIsRevelation))
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Window qualified as Window
import Control.Monad.Trans.Class

import Arkham.Helpers.SkillTest as X hiding (
  beginSkillTest,
  beginSkillTestEdit,
  cancelTokenDraw,
  evade,
  exploreTest,
  fight,
  investigate,
  isSkillTestInvestigator,
  isSkillTestSource,
  parley,
  pushAfterSkillTest,
  revelationSkillTest,
 )
import Arkham.Helpers.SkillTest qualified as Msg

revelationSkillTest
  :: (Sourceable source, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> SkillType
  -> GameCalculation
  -> m ()
revelationSkillTest sid iid source sType calc = beginSkillTestEdit sid iid source (sourceToTarget $ toSource source) sType calc \st -> st {skillTestIsRevelation = True}

beginSkillTest
  :: (Sourceable source, Targetable target, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> m ()
beginSkillTest sid iid source target sType n = push $ Msg.beginSkillTest sid iid source target sType n

beginSkillTestEdit
  :: (Sourceable source, Targetable target, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> (SkillTest -> SkillTest)
  -> m ()
beginSkillTestEdit sid iid source target sType n f = push $ Msg.beginSkillTestEdit sid iid source target sType n f

parley
  :: (Sourceable source, Targetable target, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> m ()
parley sid iid source target sType n = push $ Msg.parley sid iid source target sType n

exploreTest
  :: (Sourceable source, Targetable target, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> m ()
exploreTest sid iid source target sType n = push $ Msg.exploreTest sid iid source target sType n

fight
  :: (Sourceable source, Targetable target, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> m ()
fight sid iid source target sType n = push $ Msg.fight sid iid source target sType n

evade
  :: (Sourceable source, Targetable target, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> m ()
evade sid iid source target sType n = push $ Msg.evade sid iid source target sType n

investigate_
  :: (Sourceable source, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> m ()
investigate_ sid iid source = investigateEdit_ sid iid source id

investigateEdit_
  :: (Sourceable source, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> (I.Investigate -> I.Investigate)
  -> m ()
investigateEdit_ sid iid source f = withLocationOf iid (push . toMessage . f <=< mkInvestigateLocation sid iid source)

investigateLocationEdit_
  :: (Sourceable source, ReverseQueue m, AsId location, IdOf location ~ LocationId)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> location
  -> (I.Investigate -> I.Investigate)
  -> m ()
investigateLocationEdit_ sid iid source lid f = push . toMessage . f =<< mkInvestigateLocation sid iid source (asId lid)

investigateLocation_
  :: (Sourceable source, ReverseQueue m, AsId location, IdOf location ~ LocationId)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> location
  -> m ()
investigateLocation_ sid iid source lid = investigateLocationEdit_ sid iid source lid id

investigate
  :: (Sourceable source, Targetable target, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> m ()
investigate sid iid source target sType n = push $ Msg.investigate sid iid source target sType n

pushAfterSkillTest :: (MonadTrans t, HasQueue Message m) => Message -> t m ()
pushAfterSkillTest =
  lift . pushAfter \case
    SkillTestEnds {} -> True
    _ -> False

cancelTokenDraw :: (MonadTrans t, HasQueue Message m) => t m ()
cancelTokenDraw = lift $ do
  let
    removeWindow window = case window.kind of
      Window.WouldRevealChaosToken {} -> True
      Window.WouldRevealChaosTokens {} -> True
      _ -> False
  popMessageMatching_ $ \case
    CheckWindows windows' -> any removeWindow windows'
    Do (CheckWindows windows') -> any removeWindow windows'
    _ -> False
  popMessageMatching_ $ \case
    NextChaosBagStep {} -> True
    _ -> False
  popMessageMatching_ $ \case
    RunBag {} -> True
    _ -> False
  popMessageMatching_ $ \case
    RunSkillTest {} -> True
    _ -> False

isSkillTestSource
  :: (Sourceable source, HasGame m, Alternative (t m), MonadTrans t) => source -> t m ()
isSkillTestSource = liftGuardM . Msg.isSkillTestSource

isSkillTestInvestigator
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, HasGame m, Alternative (t m), MonadTrans t)
  => investigator
  -> t m ()
isSkillTestInvestigator = liftGuardM . Msg.isSkillTestInvestigator . asId
