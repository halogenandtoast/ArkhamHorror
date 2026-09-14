{- | Names for the positional fields of constructors the editor builds.

@TakeResources InvestigatorId Int Source Bool@ says nothing about what its 'Bool'
means. The editor can only show the type, so the author is offered @false@ beside
@true@ with no way to tell which is wanted -- and a 'Bool' is the one type where
guessing wrong is silent and total. There is nothing in the type to read the
answer off, so it is written down here.

Keyed by type /and/ constructor: a name is not unique on its own. @PlayCard@ and
@MoveAction@ are both a 'Message' and a 'WindowMatcher', and naming the message's
fields on the matcher's is worse than leaving it alone -- the matcher's @Who@
would be captioned "to".

Positional: one entry per field, in order, and @""@ leaves a field to its type,
which is the right answer for most of them (@InvestigatorId@ needs no gloss).
Only constructors worth annotating need an entry at all; everything else falls
back to the type as before.

A name here is a claim about the engine, so add one only after reading the
handler that consumes the field. The ones below were each taken from the runner
that pattern matches on them.
-}
module Arkham.Custom.Schema.Fields (conFieldNames) where

import Arkham.Prelude

conFieldNames :: [((Text, Text), [Text])]
conFieldNames =
  [ -- True runs the whole resource *action* -- an action is spent, the window
    -- fires, an attack of opportunity is provoked. False simply gains them.
    (("Message", "TakeResources"), ["", "how many", "", "as the resource action"])
  , -- Same shape: True is the move action, False is bare movement.
    (("Message", "MoveAction"), ["", "to", "", "as the move action"])
  , -- `asAction` in Investigator.Runner: whether playing it spends an action.
    (("Message", "PlayCard"), ["", "", "target", "", "windows", "as an action"])
  , (("Message", "InitiatePlayCard"), ["", "", "target", "", "windows", "as an action"])
  , (("Message", "InitiatePlayCardWithWindows"), ["", "", "target", "", "windows", "as an action"])
  , -- `isFast` in Enemy.Runner: a fast action provokes nothing.
    (("Message", "CheckAttackOfOpportunity"), ["", "is fast", "only from"])
  , -- `addToRemovedFromGame` in Game.Runner.
    (("Message", "RemovePlayerCardFromGame"), ["remember it was removed", ""])
  , (("Message", "SetCardSilenced"), ["", "", "silenced"])
  , (("Message", "SetFlippable"), ["", "flippable"])
  , (("Message", "SetAsIfAtIgnored"), ["", "ignored"])
  , (("Message", "SetUltimatumsAndBoonsEnabled"), ["enabled"])
  , (("Message", "SetGameRunWindows"), ["enabled"])
  , (("Message", "SetPerformTarotReadings"), ["enabled"])
  ]
