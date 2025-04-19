module Arkham.Helpers.FlavorText (module Arkham.Helpers.FlavorText, module X) where

import Arkham.FlavorText as X (li)
import Arkham.FlavorText qualified as FT
import Arkham.I18n
import Arkham.I18n as X (unscoped, withVars, scope)
import Arkham.Message.Lifted (story)
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Text
import Control.Monad.State.Strict

setup :: (HasI18n, ReverseQueue m) => (HasI18n => FlavorTextBuilder ()) -> m ()
setup body = scope "setup" $ flavor do
  unscoped $ setTitle "setup"
  body

flavor :: (HasI18n, ReverseQueue m) => (HasI18n => FlavorTextBuilder ()) -> m ()
flavor body = story $ execState (runStoryBuilder body) mempty

newtype FlavorTextBuilder a = FlavorTextBuilder {runStoryBuilder :: State FlavorText a}
  deriving newtype (Functor, Applicative, Monad, MonadState FlavorText)

setTitle :: HasI18n => Text -> FlavorTextBuilder ()
setTitle t = modify \s -> s {flavorTitle = Just ("$" <> FT.ikey t)}

h :: HasI18n => Scope -> FlavorTextBuilder ()
h = addEntry . FT.h

p :: HasI18n => Scope -> FlavorTextBuilder ()
p = addEntry . FT.p

ul :: FT.UlItems -> FlavorTextBuilder ()
ul = addEntry . FT.ul

addEntry :: FlavorTextEntry -> FlavorTextBuilder ()
addEntry entry = modify \s@FlavorText {flavorBody} -> s {flavorBody = flavorBody <> [entry]}
