module Arkham.Homebrew.DarkMatter.Agendas.TheThirdAct (theThirdAct) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (crossOffMemories)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Matcher
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

{- | Like every Machine in Yellow agenda, this prints:

"[reaction] When you would take any amount of horror: You may cross out 1 tally
mark from your 'Memories' instead."
-}
newtype TheThirdAct = TheThirdAct AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdAct :: AgendaCard TheThirdAct
theThirdAct = agenda (1, A) TheThirdAct Cards.theThirdAct (Static 4)

instance HasAbilities TheThirdAct where
  getAbilities (TheThirdAct a) =
    [ restricted a 1 (youExist $ investigatorWithRecordCount Memories (atLeast 1))
        $ freeReaction
        $ InvestigatorWouldTakeHorror #when You AnySource
    ]

instance RunMessage TheThirdAct where
  runMessage msg a@(TheThirdAct attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.WouldTakeHorror _ (InvestigatorTarget iid') n -> push $ CancelHorror iid' n
        _ -> pure ()
      crossOffMemories iid 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheThirdAct <$> liftRunMessage msg attrs
