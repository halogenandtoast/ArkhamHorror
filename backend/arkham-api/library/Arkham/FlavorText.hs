{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.FlavorText (
  module X,
  flavorText,
  ul,
  li,
  compose,
  liGatherSets,
  liOrReturnTo,
  liReturnToInstead,
  onReturnTo,
  h,
  h_,
  h1,
  h3,
  hr,
  p,
  cols,
  img,
  smallImg,
  chaosTokenImg,
  chaosTokenMorph,
  UlItems,
)
where

import Arkham.I18n as X
import Arkham.Text as X

import Arkham.Card.CardCode
import Arkham.ChaosToken.Types (ChaosTokenFace)
import Arkham.Prelude
import Control.Monad.Writer.Strict
import Data.Text qualified as T
import GHC.Records

flavorText :: FlavorTextEntry -> FlavorText
flavorText = FlavorText Nothing . pure

type UlItems = Writer [ListItemEntry] ()

ul :: UlItems -> FlavorTextEntry
ul = ListEntry . execWriter

h :: HasI18n => Scope -> FlavorTextEntry
h = h_ 1

h_ :: HasI18n => Int -> Scope -> FlavorTextEntry
h_ n t = HeaderEntry n (intercalate "." (?scope <> [t]))

h1 :: HasI18n => Scope -> FlavorTextEntry
h1 = h_ 1

h3 :: HasI18n => Scope -> FlavorTextEntry
h3 = h_ 3

p :: HasI18n => Scope -> FlavorTextEntry
p = i18nEntry

compose :: [FlavorTextEntry] -> FlavorTextEntry
compose = CompositeEntry

cols :: [FlavorTextEntry] -> FlavorTextEntry
cols = ColumnEntry

img :: CardCode -> FlavorTextEntry
img = (`CardEntry` [])

smallImg :: CardCode -> FlavorTextEntry
smallImg = (`CardEntry` [SmallImage])

chaosTokenImg :: ChaosTokenFace -> FlavorTextEntry
chaosTokenImg = ChaosTokenEntry

chaosTokenMorph :: ChaosTokenFace -> ChaosTokenFace -> FlavorTextEntry
chaosTokenMorph = ChaosTokenMorphEntry

hr :: FlavorTextEntry
hr = EntrySplit

li :: HasI18n => Text -> UlItems
li t = tell [ListItemEntry (i18nEntry t) []]

{- | A setup line a "Return to" box rewrites. The box's wording lives beside the
original under @returnTo@ in the same locale file and stands in for it, so it carries
no change marker of its own -- what changed inside the sentence is marked there with
@.return-to-swap@ (the encounter sets the box adds or replaces, and their icons).
-}
liOrReturnTo :: HasI18n => Bool -> Text -> UlItems
liOrReturnTo isReturnTo t = if isReturnTo then scope "returnTo" (li t) else li t

{- | A setup line a "Return to" box replaces outright: the Campaign Guide's for a normal
game, the box's wording -- marked as a change -- for a "Return to" one.
-}
liReturnToInstead :: HasI18n => Bool -> Text -> UlItems
liReturnToInstead isReturnTo t =
  if isReturnTo then scope "returnTo" (li.returnTo (T.unpack t)) else li t

{- | The @gatherSets@ line plus, in a "Return to" game, the encounter sets the box adds
on top of the Campaign Guide's list. The printed scenario card reads "perform the setup
as indicated in the Campaign Guide, with the following exceptions: when gathering
encounter sets, also gather the new encounter sets shown here" -- so the original line
stays exactly as it is and the box's sets hang under it, with their icons.
-}
liGatherSets :: HasI18n => Bool -> UlItems
liGatherSets isReturnTo =
  li.nested "gatherSets" $ onReturnTo isReturnTo $ li.returnTo "gatherSets"

{- | The setup lines a "Return to" box adds. Written in the position of the instruction
they modify and marked as changes, so the list reads as the Campaign Guide's own with
the box's exceptions called out in place. Keys come from the scenario's @returnTo@
locale block.
-}
onReturnTo :: HasI18n => Bool -> (HasI18n => UlItems) -> UlItems
onReturnTo isReturnTo body = when isReturnTo $ scope "returnTo" body

specialize :: (Text -> UlItems) -> Text -> (ListItemEntry -> ListItemEntry) -> UlItems
specialize f t convert = tell $ map convert $ execWriter $ f t

specializeS :: (String -> UlItems) -> String -> (ListItemEntry -> ListItemEntry) -> UlItems
specializeS f t convert = tell $ map convert $ execWriter $ f t

instance HasField "nested" (Text -> UlItems) (String -> UlItems -> UlItems) where
  getField f t items = specialize f (T.pack t) \case
    ListItemEntry entry nested -> ListItemEntry entry (nested <> execWriter items)

instance HasField "nested" (String -> UlItems) (String -> UlItems -> UlItems) where
  getField f t items = specializeS f t \case
    ListItemEntry entry nested -> ListItemEntry entry (nested <> execWriter items)

instance HasField "byDifficulty" (String -> UlItems -> UlItems) (String -> UlItems -> UlItems) where
  getField f t items =
    tell $ execWriter (f t items) & map \case
      ListItemEntry entry nested -> case entry of
        ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (ByDifficultyEntry : modifiers) inner) nested
        _ -> ListItemEntry (ModifyEntry [ByDifficultyEntry] entry) nested

{- | Mark a setup line as a change this box makes to the scenario it wraps. An
unofficial "Return to" scenario card reads as the original list with its own deltas
called out, so the lines it adds or rewrites are the ones the reader has to spot.
-}
instance HasField "returnTo" (Text -> UlItems) (String -> UlItems) where
  getField f t = specialize f (T.pack t) \case
    ListItemEntry entry nested -> ListItemEntry (extendModifiers ReturnToEntry entry) nested

instance HasField "returnTo" (String -> UlItems -> UlItems) (String -> UlItems -> UlItems) where
  getField f t items =
    tell $ execWriter (f t items) & map \case
      ListItemEntry entry nested -> ListItemEntry (extendModifiers ReturnToEntry entry) nested

instance HasField "valid" (Text -> UlItems) (String -> UlItems) where
  getField f t = specialize f (T.pack t) \case
    ListItemEntry entry nested -> case entry of
      ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (ValidEntry : modifiers) inner) nested
      _ -> ListItemEntry (ModifyEntry [ValidEntry] entry) nested

instance HasField "byDifficulty" (Text -> UlItems) (String -> UlItems) where
  getField f t = specialize f (T.pack t) \case
    ListItemEntry entry nested -> case entry of
      ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (ByDifficultyEntry : modifiers) inner) nested
      _ -> ListItemEntry (ModifyEntry [ByDifficultyEntry] entry) nested

instance HasField "invalid" (Text -> UlItems) (String -> UlItems) where
  getField f t = specialize f (T.pack t) \case
    ListItemEntry entry nested -> case entry of
      ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (InvalidEntry : modifiers) inner) nested
      _ -> ListItemEntry (ModifyEntry [InvalidEntry] entry) nested

instance HasField "validate" (String -> UlItems -> UlItems) (Bool -> String -> UlItems -> UlItems) where
  getField f cond t items =
    let modifier = if cond then ValidEntry else InvalidEntry
     in specializeS (`f` items) t \case
          ListItemEntry entry nested -> case entry of
            ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (modifier : modifiers) inner) nested
            _ -> ListItemEntry (ModifyEntry [modifier] entry) nested

instance HasField "validate" (Text -> UlItems) (Bool -> String -> UlItems) where
  getField f cond t =
    let modifier = if cond then ValidEntry else InvalidEntry
     in specialize f (T.pack t) \case
          ListItemEntry entry nested -> case entry of
            ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (modifier : modifiers) inner) nested
            _ -> ListItemEntry (ModifyEntry [modifier] entry) nested

instance HasField "remove" (CardCode -> FlavorTextEntry) (CardCode -> FlavorTextEntry) where
  getField f cardCode = case f cardCode of
    CardEntry _ modifiers -> CardEntry cardCode (RemoveImage : modifiers)
    other -> other

extendModifiers :: FlavorTextModifier -> FlavorTextEntry -> FlavorTextEntry
extendModifiers modifier = \case
  ModifyEntry modifiers inner -> ModifyEntry (modifier : modifiers) inner
  other -> ModifyEntry [modifier] other

instance HasField "blue" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers BlueEntry . f

instance HasField "green" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers GreenEntry . f

instance HasField "codex" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers CodexEntry . f

instance HasField "bordered" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers BorderedEntry . f

instance HasField "red" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers RedEntry . f

instance HasField "interlude" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers InterludeEntry . f

instance HasField "right" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers RightAligned . f

instance HasField "nested" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers NestedEntry . f

instance HasField "validate" (Scope -> FlavorTextEntry) (Bool -> Scope -> FlavorTextEntry) where
  getField f cond = extendModifiers (if cond then ValidEntry else InvalidEntry) . f
