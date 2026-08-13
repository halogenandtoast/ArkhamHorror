module Arkham.Homebrew.DarkMatter.Agendas.ANightmare (aNightmare) where

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
newtype ANightmare = ANightmare AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aNightmare :: AgendaCard ANightmare
aNightmare = agenda (2, A) ANightmare Cards.aNightmare (Static 5)

instance HasAbilities ANightmare where
  getAbilities (ANightmare a) =
    [ restricted a 1 (youExist $ investigatorWithRecordCount Memories (atLeast 1))
        $ freeReaction
        $ InvestigatorWouldTakeHorror #when You AnySource
    ]

instance RunMessage ANightmare where
  runMessage msg a@(ANightmare attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.WouldTakeHorror _ (InvestigatorTarget iid') n -> push $ CancelHorror iid' n
        _ -> pure ()
      crossOffMemories iid 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> ANightmare <$> liftRunMessage msg attrs
