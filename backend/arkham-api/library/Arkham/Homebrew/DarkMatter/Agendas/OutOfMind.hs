module Arkham.Homebrew.DarkMatter.Agendas.OutOfMind (outOfMind) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.MachineInYellow
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

{- | Besides the reaction every Machine in Yellow agenda prints, this one adds:

"Forced - After you add doom to any card in play (including this agenda): Each
investigator takes 2 direct horror."
-}
newtype OutOfMind = OutOfMind AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Out of Mind has no doom threshold. Its printed back reads "There is no reason
to flip to agenda 3b", so doom simply accumulates on it.
-}
outOfMind :: AgendaCard OutOfMind
outOfMind =
  agendaWith (3, A) OutOfMind Cards.outOfMind (Static 0)
    $ doomThresholdL
    .~ Nothing

instance HasAbilities OutOfMind where
  getAbilities (OutOfMind a) =
    [ memoriesInsteadOfHorror a
    , mkAbility a 2 $ forced $ PlacedDoomCounter #after AnySource AnyTarget
    ]

instance RunMessage OutOfMind where
  runMessage msg a@(OutOfMind attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      crossOffMemoriesInsteadOfHorror iid ws
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      eachInvestigator \iid -> directHorror iid (attrs.ability 2) 2
      pure a
    -- The back is a joke with no in-game way to reach it, so the Konami code is
    -- the only thing that flips it. Its reward is offered once per game.
    KonamiCode _ -> do
      unless (toResultDefault False attrs.meta) $ advanceAgenda attrs
      pure a
    {- Agenda 3b:

    "You may heal up to 1 mental trauma. (Group limit once per game.)
    ... Flip back to agenda 3a." -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      wounded <- select InvestigatorWithMentalTrauma
      chooseOneM lead $ withI18n $ countVar 1 do
        targets wounded \iid -> push $ HealTrauma iid 0 1
        labeled' "doNotHeal" nothing
      revertAgenda attrs
      pure . overAttrs (setMeta True) $ a
    _ -> OutOfMind <$> liftRunMessage msg attrs
