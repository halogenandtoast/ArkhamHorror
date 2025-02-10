module Arkham.Message.Lifted.Action where

import Arkham.Ability.Types
import Arkham.Action
import Arkham.Classes.HasQueue
import Arkham.Constants
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Window
import Arkham.Window qualified as Window
import Control.Monad.Trans.Class

isTakenActions :: Message -> Bool
isTakenActions = \case
  TakenActions {} -> True
  _ -> False

isCheckWindows :: Message -> Bool
isCheckWindows = \case
  CheckWindows ws -> any isCheckWindow ws
  _ -> False

isCheckWindow :: Window -> Bool
isCheckWindow w = w.timing == #after && isActivateAbility
 where
  isActivateAbility = case w.kind of
    Window.ActivateAbility _ _ ab -> ab.index == PlayAbility
    _ -> False

replaceWindow :: [Window] -> [Window]
replaceWindow [] = []
replaceWindow (w : ws) = case w.kind of
  Window.ActivateAbility iid source ab
    | ab.index == PlayAbility ->
        w {windowType = Window.ActivateAbility iid source (overAbilityActions (nub . (#fight :)) ab)} : ws
  _ -> w : replaceWindow ws

extendTakenActions :: (MonadTrans t, HasQueue Message m) => [Action] -> t m ()
extendTakenActions as = do
  insteadOfMatchingWith isTakenActions \case
    TakenActions iid' as' -> pure [TakenActions iid' (nub $ as' <> as)]
    _ -> error "no match"
  insteadOfMatchingWith isCheckWindows \case
    CheckWindows ws -> pure [CheckWindows $ replaceWindow ws]
    _ -> error "no match"
