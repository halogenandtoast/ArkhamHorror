module Arkham.Helpers.SkillTest.Lifted (module Arkham.Helpers.SkillTest.Lifted, module X) where

import Arkham.Calculation
import Arkham.Classes.HasQueue
import Arkham.Id
import Arkham.Message (Message (..))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Window qualified as Window
import Control.Monad.Trans.Class

import Arkham.Helpers.SkillTest as X hiding (
  beginSkillTest,
  cancelTokenDraw,
  evade,
  exploreTest,
  fight,
  investigate,
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
revelationSkillTest sid iid source sType calc = push $ Msg.revelationSkillTest sid iid source sType calc

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
      _ -> False
  popMessageMatching_ $ \case
    RunWindow _ windows' -> any removeWindow windows'
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
