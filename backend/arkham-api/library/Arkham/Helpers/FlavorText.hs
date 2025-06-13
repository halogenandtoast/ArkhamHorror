module Arkham.Helpers.FlavorText (module Arkham.Helpers.FlavorText, module X) where

import Arkham.Classes.HasQueue (push)
import Arkham.FlavorText as X (li)
import Arkham.FlavorText qualified as FT
import Arkham.Helpers.Query (allPlayers)
import Arkham.I18n
import Arkham.I18n as X (countVar, scope, unscoped, withVar, withVars, HasI18n)
import Arkham.Id
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted (story)
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Text
import Control.Monad.State.Strict
import GHC.Records

setup :: (HasI18n, ReverseQueue m) => (HasI18n => FlavorTextBuilder ()) -> m ()
setup body = scope "setup" $ flavor do
  unscoped $ setTitle "setup"
  body

flavor :: (HasI18n, ReverseQueue m) => (HasI18n => FlavorTextBuilder ()) -> m ()
flavor builder = story $ buildFlavor builder

buildFlavor :: FlavorTextBuilder () -> FlavorText
buildFlavor body = execState (runStoryBuilder body) mempty

newtype FlavorTextBuilder a = FlavorTextBuilder {runStoryBuilder :: State FlavorText a}
  deriving newtype (Functor, Applicative, Monad, MonadState FlavorText)

setTitle :: HasI18n => Text -> FlavorTextBuilder ()
setTitle t = modify \s -> s {flavorTitle = Just ("$" <> FT.ikey t)}

h :: HasI18n => Scope -> FlavorTextBuilder ()
h t = setTitle t >> h_ t

h_ :: HasI18n => Scope -> FlavorTextBuilder ()
h_ = addEntry . FT.h

p :: HasI18n => Scope -> FlavorTextBuilder ()
p = addEntry . FT.p

ul :: FT.UlItems -> FlavorTextBuilder ()
ul = addEntry . FT.ul

compose :: FlavorTextBuilder () -> FlavorTextBuilder ()
compose builder = case buildFlavor builder of
  FlavorText _t b -> addEntry $ CompositeEntry b

addEntry :: FlavorTextEntry -> FlavorTextBuilder ()
addEntry entry = modify \s@FlavorText {flavorBody} -> s {flavorBody = flavorBody <> [entry]}

storyBuild :: ReverseQueue m => FlavorTextBuilder () -> m ()
storyBuild builder = do
  players <- allPlayers
  push $ Msg.story players (buildFlavor builder)

instance HasField "validate" (Scope -> FlavorTextBuilder ()) (Bool -> Scope -> FlavorTextBuilder ()) where
  getField f cond t = do
    let modifier = if cond then ValidEntry else InvalidEntry
    for_ (buildFlavor (f t)).flavorBody \case
      ModifyEntry mods inner' -> addEntry $ ModifyEntry (modifier : mods) inner'
      inner' -> addEntry $ ModifyEntry [modifier] inner'

instance HasField "valid" (Scope -> FlavorTextBuilder ()) (Scope -> FlavorTextBuilder ()) where
  getField f t = for_ (buildFlavor (f t)).flavorBody \case
    ModifyEntry mods inner' -> addEntry $ ModifyEntry (ValidEntry : mods) inner'
    inner' -> addEntry $ ModifyEntry [ValidEntry] inner'

instance HasField "invalid" (Scope -> FlavorTextBuilder ()) (Scope -> FlavorTextBuilder ()) where
  getField f t = for_ (buildFlavor (f t)).flavorBody \case
    ModifyEntry mods inner' -> addEntry $ ModifyEntry (InvalidEntry : mods) inner'
    inner' -> addEntry $ ModifyEntry [InvalidEntry] inner'

instance HasField "right" (Scope -> FlavorTextBuilder ()) (Scope -> FlavorTextBuilder ()) where
  getField f t = for_ (buildFlavor (f t)).flavorBody \case
    ModifyEntry mods inner' -> addEntry $ ModifyEntry (RightAligned : mods) inner'
    inner' -> addEntry $ ModifyEntry [RightAligned] inner'

instance HasField "blue" (Scope -> FlavorTextBuilder ()) (Scope -> FlavorTextBuilder ()) where
  getField f t = for_ (buildFlavor (f t)).flavorBody \case
    ModifyEntry mods inner' -> addEntry $ ModifyEntry (BlueEntry : mods) inner'
    inner' -> addEntry $ ModifyEntry [BlueEntry] inner'

instance HasField "green" (Scope -> FlavorTextBuilder ()) (Scope -> FlavorTextBuilder ()) where
  getField f t = for_ (buildFlavor (f t)).flavorBody \case
    ModifyEntry mods inner' -> addEntry $ ModifyEntry (GreenEntry : mods) inner'
    inner' -> addEntry $ ModifyEntry [GreenEntry] inner'

instance HasField "interlude" (Scope -> FlavorTextBuilder ()) (Scope -> FlavorTextBuilder ()) where
  getField f t = for_ (buildFlavor (f t)).flavorBody \case
    ModifyEntry mods inner' -> addEntry $ ModifyEntry (InterludeEntry : mods) inner'
    inner' -> addEntry $ ModifyEntry [InterludeEntry] inner'

instance
  HasField
    "green"
    (FlavorTextBuilder () -> FlavorTextBuilder ())
    (FlavorTextBuilder () -> FlavorTextBuilder ())
  where
  getField f builder = for_ (buildFlavor $ f builder).flavorBody \case
    ModifyEntry mods inner' -> addEntry $ ModifyEntry (GreenEntry : mods) inner'
    inner' -> addEntry $ ModifyEntry [GreenEntry] inner'

storyOnlyBuild :: ReverseQueue m => [InvestigatorId] -> FlavorTextBuilder () -> m ()
storyOnlyBuild [] _ = pure ()
storyOnlyBuild iids builder = do
  players <- traverse getPlayer iids
  push $ Msg.story players (buildFlavor builder)
