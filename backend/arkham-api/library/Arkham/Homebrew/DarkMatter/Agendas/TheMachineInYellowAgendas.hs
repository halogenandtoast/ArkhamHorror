module Arkham.Homebrew.DarkMatter.Agendas.TheMachineInYellowAgendas (
  aNightmare,
  outOfMind,
  theThirdAct,
) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card (toCardCode)
import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (crossOffMemories)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Matcher
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

{- | All three Machine in Yellow agendas print:

"[reaction] When you would take any amount of horror: You may cross out 1 tally
mark from your 'Memories' instead."

Out of Mind additionally prints "Forced - After you add doom to any card in play
(including this agenda): Each investigator takes 2 direct horror."
-}
newtype TheMachineInYellowAgenda = TheMachineInYellowAgenda AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdAct :: AgendaCard TheMachineInYellowAgenda
theThirdAct = agenda (1, A) TheMachineInYellowAgenda Cards.theThirdAct (Static 4)

aNightmare :: AgendaCard TheMachineInYellowAgenda
aNightmare = agenda (2, A) TheMachineInYellowAgenda Cards.aNightmare (Static 5)

{- | Out of Mind has no doom threshold. Its printed back reads "There is no reason
to flip to agenda 3b."
(@docs/homebrew/data/dark-matter-sets/the_machine_in_yellow.md@), so doom simply
accumulates on it.
-}
outOfMind :: AgendaCard TheMachineInYellowAgenda
outOfMind =
  agendaWith (3, A) TheMachineInYellowAgenda Cards.outOfMind (Static 0)
    $ doomThresholdL
    .~ Nothing

isOutOfMind :: AgendaAttrs -> Bool
isOutOfMind a = toCardCode (toCardDef a) == toCardCode (Cards.outOfMind :: CardDef)

instance HasAbilities TheMachineInYellowAgenda where
  getAbilities (TheMachineInYellowAgenda a) =
    [ restricted a 1 (youExist $ investigatorWithRecordCount Memories (atLeast 1))
        $ freeReaction
        $ InvestigatorWouldTakeHorror #when You AnySource
    ]
      <> [mkAbility a 2 $ forced $ PlacedDoomCounter #after AnySource AnyTarget | isOutOfMind a]

instance RunMessage TheMachineInYellowAgenda where
  runMessage msg a@(TheMachineInYellowAgenda attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.WouldTakeHorror _ (InvestigatorTarget iid') n -> push $ CancelHorror iid' n
        _ -> pure ()
      crossOffMemories iid 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      eachInvestigator \iid -> directHorror iid (attrs.ability 2) 2
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheMachineInYellowAgenda <$> liftRunMessage msg attrs
