module Arkham.Homebrew.AgesUnwound.Treacheries.TimesSurge (timesSurge) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TimesSurge = TimesSurge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timesSurge :: TreacheryCard TimesSurge
timesSurge = treachery TimesSurge Cards.timesSurge

getDrawer :: [Window] -> Maybe InvestigatorId
getDrawer = \case
  [] -> Nothing
  ((windowType -> Window.WouldDrawEncounterCard iid _ _) : _) -> Just iid
  (_ : ws) -> getDrawer ws

{- | "[reaction] When an investigator would draw the top card of the encounter
deck, discard Time's Surge: That investigator gains an action instead."

The reaction belongs to whoever has it in their threat area, but any
investigator's draw opens it.
-}
instance HasAbilities TimesSurge where
  getAbilities (TimesSurge a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ freeReaction
        $ WouldDrawEncounterCard #when Anyone AnyPhase
    ]

{- | "Revelation - Draw the top three cards of the encounter deck. For each copy
of Time's Ebb drawn this way, discard it and draw the top card of the encounter
deck. Then, put Time's Surge into play in your threat area."

Time's Ebb's whole revelation is "put it into play in your threat area", so
letting it land and then discarding it reaches the same board state as never
resolving it. The count taken before the draws is what tells a copy drawn this
way from one that was already in the threat area; the copies are
interchangeable, so which of them is discarded does not matter.

Time's Surge is placed last, exactly as printed: while it is still in limbo its
own @InThreatAreaOf You@ reaction cannot be used to cancel one of these three
draws.
-}
instance RunMessage TimesSurge where
  runMessage msg t@(TimesSurge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      before <- selectCount $ treacheryIs Cards.timesEbb <> treacheryInThreatAreaOf iid
      drawEncounterCards iid attrs 3
      doStep before msg
      placeInThreatArea attrs iid
      pure t
    DoStep before (Revelation iid (isSource attrs -> True)) -> do
      ebbs <- select $ treacheryIs Cards.timesEbb <> treacheryInThreatAreaOf iid
      let drawnThisWay = take (max 0 $ length ebbs - before) ebbs
      for_ drawnThisWay $ toDiscard attrs
      drawEncounterCards iid attrs (length drawnThisWay)
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawer -> mDrawer) _ -> do
      for_ mDrawer \drawer -> do
        msgs <- capture $ gainActions drawer (attrs.ability 1) 1
        push $ Instead (DoDrawCards drawer) (Run msgs)
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> TimesSurge <$> liftRunMessage msg attrs
