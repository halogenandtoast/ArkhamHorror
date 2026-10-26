{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Homebrew.AgesUnwound.Locations.RitualCircle_224 (ritualCircle_224) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.I18n (ikey)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Import.Lifted
import Arkham.Log (idText, ikeyScoped, narrative, sendLog, (~>))
import Arkham.Log.Refs (investigatorRefFor)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype RitualCircle_224 = RitualCircle_224 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /A Harnessed Future/. Set aside; Scenario III puts it into play.
ritualCircle_224 :: LocationCard RitualCircle_224
ritualCircle_224 = symbolLabel $ location RitualCircle_224 Cards.ritualCircle_224 4 (PerPlayer 1)

{- | An investigator this location has taken out of the game, and everything the
suspended turn has to hand back when it resumes.

A suspended turn is not re-entered -- the engine has no way to -- so what makes
the return a /resumed/ turn rather than a fresh one is the actions: the round
the seat comes back in re-derives @remainingActions@ and clears
@usedAdditionalActions@ for everyone, so both the count and the still-unspent
additional actions have to travel across with the seat. See 'resumeActions'.
-}
data Suspension = Suspension
  { investigator :: InvestigatorId
  , actionsRemaining :: Maybe Int
  , additional :: [AdditionalAction]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | A seat suspended by an older version of this card was stored without one or
both halves of its action snapshot (first as a bare id, later as an id and a
count). Such a seat still comes back -- with an ordinary turn, since there is
nothing to resume it to -- rather than being dropped by a decode failure and
stranded out of the game forever.
-}
instance FromJSON Suspension where
  parseJSON v = withObject "Suspension" obj v <|> bareId
   where
    obj o = Suspension <$> o .: "investigator" <*> o .:? "actionsRemaining" <*> (o .:? "additional" .!= [])
    bareId = Suspension <$> parseJSON v <*> pure Nothing <*> pure []

{- | Who this location is holding, and who it is handing back.

@suspended@ are out of the game, pending their return at the start of the next
investigation phase. @resuming@ are the seats that return has just handed back,
held until each takes the turn it was suspended on -- that is what
'ResumesSuspendedTurn' and the re-granted additional actions are keyed off.
-}
data Meta = Meta
  { suspended :: [Suspension]
  , resuming :: [Suspension]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

-- | Older metadata was the bare list of suspended seats.
instance FromJSON Meta where
  parseJSON v =
    withObject "Meta" (\o -> Meta <$> o .: "suspended" <*> (o .:? "resuming" .!= [])) v
      <|> (flip Meta [] <$> parseJSON v)

meta :: LocationAttrs -> Meta
meta = getLocationMetaDefault (Meta [] [])

{- | What the log says when this card hands a seat back, in place of
@"Daisy Walker moves to Ritual Circle: A Harnessed Future"@.

A return is not a move -- the investigator was out of the game, and the default
wording sends the reader hunting for the action that paid for it -- so the seat
carries 'EntersLocationNarratedAs' while it is suspended and the narrator says
this instead. A key rather than a sentence because the narrator builds the
entry; it is handed the same @investigator@ and @location@ variables the
default line uses. Through 'campaignI18n' rather than written out as a literal
path, so the campaign's scope stays in one place.
-}
returnNarration :: Text
returnNarration = campaignI18n $ ikey "ritualCircleAHarnessedFuture.returns"

{- | The colour the circle takes you in, and gives you back in.

Flavour rather than a signal, so deliberately none of the highlight palette:
the frontend spends magenta on a choice the game is waiting for, gold on an
actor and cyan on a target, and a half-second of smoke must not read as any of
those.
-}
circleGreen :: Text
circleGreen = "#57e07a"

{- | Ask the client to play the bamf: the portrait implodes into a puff of
'circleGreen' smoke on the way out, and comes back out of one.

A one-shot over the UI channel rather than a modifier the client could notice
appearing and disappearing, because __the client does not reliably see the
states in between__. Measured on a solo game: the haunted ability, the rest of
the round, the mythos phase and the return all arrived in a /single/ update, so
the investigator went from at-the-location to at-the-location with nothing
observable between. An animation has to be told, not inferred.

The investigator goes out as 'idText' -- its JSON encoding, which is what keys
@game.investigators@ and the board's @data-investigator-mini@ on the client, so
the layer can find the portrait without translating anything. @tshow@ would
wrap it in quotes.

Sent before the placement messages it describes resolve, which is also when the
portrait is still where the puff needs to draw it. The client holds its update
lock for the length of each puff, so the state these announce lands after the
smoke clears; see @SmokePuffLayer.vue@.
-}
smokePuff :: HasGameLogger m => Text -> InvestigatorId -> m ()
smokePuff direction iid =
  sendUI $ "smokePuff:" <> direction <> ":" <> idText iid <> ":" <> circleGreen

{- | While an investigator is out of the game, nothing may reach them. Several
core loops iterate investigators without checking placement, so the ban is
spelled out as modifiers (the approach recorded in
@docs/homebrew/data/ages-unwound-engine-notes.md@ for out-of-play seats).

The seat takes no part in play of its own: 'CannotTakeAction' covers action
abilities, 'CannotTriggerAbilityMatching' the fast and reaction ones, and
'CannotDrawEncounterCards' leaves them out of the mythos draw. Everything the
game does to itself -- doom onto the agenda, an agenda advancing, a treachery's
own forced text -- still happens; a generic @AnyAbility@ ban deliberately does
not reach forced abilities (@Helpers/Ability.preventedByInvestigatorModifiers@),
which is also what keeps ability 2 below able to fire and bring them back.

'EntersLocationNarratedAs' rides along for the same stretch, and only on the
suspended half: it replaces the log line for /any/ arrival while it holds, and
the one arrival a suspended seat can have is this card putting it back. See
'returnNarration'.

'ResumesSuspendedTurn' rides on the seat from suspension until the resumed turn
ends, which is what makes the turn pick up /at the start of/ the phase and
/inside/ the turn -- step 2.2.1, with no player window before it and no
@TurnBegins@: the investigation phase reads it before it queues its own steps,
so it has to be on the seat while still suspended, not only on the way back. A
seat on its way back also carries one 'GiveAdditionalAction' for every
additional action the suspended turn still had in hand. Those are re-granted from this location rather than left to their
original sources, because 'resumeActions' has already spent whatever those
sources hand out for the new round -- the snapshot is the whole truth about
what the resumed turn has left. They keep their original labels so the player
still sees which card the action came from.

The second printed line -- "cannot willingly end their turn while they have
actions remaining" -- is the same shape, so it rides here too: 'CannotEndTurn'
only takes the end-turn button off the player window, and the window is rebuilt
with fresh modifiers after every action, so the button comes back the moment
the last action is spent. @InvestigatorWithActionsRemaining@ and not the
@Standard@ variant because the card says "actions remaining", where
/Classroom/ says "standard action".
-}
instance HasModifiersFor RitualCircle_224 where
  getModifiersFor (RitualCircle_224 a) = do
    let m = meta a
    for_ m.suspended \s ->
      modified_
        a
        s.investigator
        [ ResumesSuspendedTurn
        , EntersLocationNarratedAs returnNarration
        , CannotDrawCards
        , CannotDrawEncounterCards
        , CannotGainResources
        , CannotTakeAction IsAnyAction
        , CannotTriggerAbilityMatching AnyAbility
        , CannotPlay AnyCard
        , CannotMove
        , CannotBeAttacked
        , CannotBeEngaged
        ]
    for_ m.resuming \s ->
      modified_ a s.investigator
        $ ResumesSuspendedTurn
        : [ GiveAdditionalAction (AdditionalAction lbl (IndexedSource i $ a.ability 2) kind)
          | (i, AdditionalAction lbl _ kind) <- zip [0 ..] s.additional
          ]
    modifySelect
      a
      (InvestigatorAt (be a) <> InvestigatorWithActionsRemaining (atLeast 1))
      [CannotEndTurn]

{- | "Haunted - Suspend your current turn and remove your investigator from the
game. At the start of the next investigator phase, return your investigator to
this location and resume the current turn. /
Investigators at this location cannot willingly end their turn while they have
actions remaining."

Ability 2 is the engine hook that brings them back, so it is silent -- the card
prints no second ability.

The last line is a 'CannotEndTurn' modifier; see 'HasModifiersFor' above.

/Resume the current turn/ is the action snapshot travelling with the seat plus
'ResumesSuspendedTurn' landing it back in step 2.2.1 -- before anyone else's
turn, with no @TurnBegins@ and no player window between 2.1 and 2.2, because
the turn is picked back up rather than begun.

What the engine still does not carry across the phase boundary is the turn's own
bookkeeping: @actionsTaken@, @actionsPerformed@ and every per-turn ability limit
are reset by @Do BeginRound@ before the phase is reached, so a once-per-turn
ability already used on the suspended turn is available again. Holding those
would mean snapshotting and restoring them the way the actions are.
-}
instance HasAbilities RitualCircle_224 where
  getAbilities (RitualCircle_224 a) =
    extendRevealed a
      $ campaignI18n (hauntedI "ritualCircleAHarnessedFuture.haunted" a 1)
      : [ mkAbility a 2 $ SilentForcedAbility $ PhaseBegins #when #investigation
        | notNull (meta a).suspended
        ]

{- | Put the suspended turn's actions back.

Safe here and nowhere earlier: @Do BeginRound@ has already given everyone their
actions for the new round and @BeginTurn@ will not touch the count again, so
this is what the investigator takes their turn with.

@SetActions _ 0@ is the engine's "you have no actions left", which zeroes the
count /and/ marks every additional action a live source is currently offering
as spent -- so the seat is stripped back to nothing before the snapshot is put
back on top of it, and a source that survived the suspension (Leo De Luca, say)
cannot hand out a second copy of an action the suspended turn had already used.
The standard count then goes back with a second 'setActions'; the additional
actions go back as modifiers, see 'HasModifiersFor'.

A seat recorded before the snapshot existed has nothing to put back and takes
an ordinary turn.
-}
resumeActions :: ReverseQueue m => LocationAttrs -> Suspension -> m ()
resumeActions attrs s = for_ s.actionsRemaining \n -> do
  setActions s.investigator (attrs.ability 2) 0
  when (n > 0) $ setActions s.investigator (attrs.ability 2) n

instance RunMessage RitualCircle_224 where
  runMessage msg l@(RitualCircle_224 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Read before the turn is taken away, or there is nothing left to resume.
      remaining <- field InvestigatorRemainingActions iid
      unspent <- field InvestigatorAdditionalActions iid
      setActions iid (attrs.ability 1) 0
      endYourTurn iid
      push $ PlaceInvestigator iid Unplaced
      {- The seat leaving the game is the whole point of the ability and the
      engine narrates none of it: @PlaceInvestigator _ Unplaced@ is silent, so
      without this the log shows a haunted ability firing and then nothing at
      all until the investigator turns up again a phase later. Sent here rather
      than queued so it lands with the ability that caused it. -}
      who <- investigatorRefFor iid
      campaignI18n
        $ sendLog
        $ narrative [ikeyScoped "ritualCircleAHarnessedFuture.removed" ["investigator" ~> who]]
      smokePuff "out" iid
      let m = meta attrs
      let kept = filter ((/= iid) . (.investigator)) m.suspended
      pure
        . RitualCircle_224
        $ attrs
        & setMeta m {suspended = Suspension iid (Just remaining) unspent : kept}
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      for_ (meta attrs).suspended \s -> do
        smokePuff "in" s.investigator
        push $ PlaceInvestigator s.investigator (AtLocation attrs.id)
        resumeActions attrs s
      -- The seats only become @resuming@ once 'resumeActions' has run, because
      -- the additional actions it re-grants are modifiers from this location
      -- and @SetActions _ 0@ would count them among the ones it spends.
      doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 2) -> do
      pure . RitualCircle_224 $ attrs & setMeta (Meta [] (meta attrs).suspended)
    -- The turn it was suspended on has now been taken, so the seat stops
    -- claiming the first turn and the re-granted actions go away with it.
    EndTurn iid | any ((== iid) . (.investigator)) (meta attrs).resuming -> do
      let m = meta attrs
      let kept = filter ((/= iid) . (.investigator)) m.resuming
      pure . RitualCircle_224 $ attrs & setMeta m {resuming = kept}
    _ -> RitualCircle_224 <$> liftRunMessage msg attrs
