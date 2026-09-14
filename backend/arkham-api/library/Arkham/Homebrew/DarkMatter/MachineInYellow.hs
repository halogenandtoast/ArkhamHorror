module Arkham.Homebrew.DarkMatter.MachineInYellow (
  memoriesInsteadOfHorror,
  crossOffMemoriesInsteadOfHorror,
  resolveHiddenForcedEffects,
) where

import Arkham.Ability
import Arkham.Agenda.Types (AgendaAttrs)
import Arkham.Card (toCardId)
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (select, selectField)
import Arkham.Homebrew.DarkMatter.Helpers (crossOffMemories)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (UseCardAbility), pattern CancelHorror)
import Arkham.Message.Lifted (ReverseQueue)
import Arkham.Prelude
import Arkham.Target
import Arkham.Treachery.Types (Field (TreacheryCard))
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

{- | Every Machine in Yellow agenda prints the same reaction as its first
ability:

"[reaction] When you would take any amount of horror: You may cross out 1 tally
mark from your \"Memories\" instead."
-}
memoriesInsteadOfHorror :: AgendaAttrs -> Ability
memoriesInsteadOfHorror a =
  restricted a 1 (youExist $ investigatorWithRecordCount Memories (atLeast 1))
    $ freeReaction
    $ InvestigatorWouldTakeHorror #when You AnySource

-- | The whole amount is replaced, however large it is, for one tally mark.
crossOffMemoriesInsteadOfHorror :: ReverseQueue m => InvestigatorId -> [Window] -> m ()
crossOffMemoriesInsteadOfHorror iid ws = do
  for_ ws \w -> case windowType w of
    Window.WouldTakeHorror _ (InvestigatorTarget iid') n -> push $ CancelHorror iid' n
    _ -> pure ()
  crossOffMemories iid 1

{- | The Stranger's "Resolve the Forced effect of each hidden card in your hand
(ignoring the conditions)". The forced effects are looked up rather than assumed
to sit at a fixed ability index, and pushing the ability directly is what
ignores its conditions.
-}
resolveHiddenForcedEffects :: ReverseQueue m => InvestigatorId -> m ()
resolveHiddenForcedEffects iid = do
  cards <- selectField TreacheryCard $ HiddenTreachery <> TreacheryInHandOf (InvestigatorWithId iid)
  abilities <-
    select $ AbilityIsForcedAbility <> AbilityOnCard (mapOneOf (CardWithId . toCardId) cards)
  for_ abilities \ab -> push $ UseCardAbility iid ab.source ab.index [] NoPayment
