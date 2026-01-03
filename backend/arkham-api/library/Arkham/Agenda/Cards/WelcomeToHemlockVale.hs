module Arkham.Agenda.Cards.WelcomeToHemlockVale (welcomeToHemlockVale) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher

newtype WelcomeToHemlockVale = WelcomeToHemlockVale AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

welcomeToHemlockVale :: AgendaCard WelcomeToHemlockVale
welcomeToHemlockVale = agenda (1, A) WelcomeToHemlockVale Cards.welcomeToHemlockVale (Static 6)

instance HasAbilities WelcomeToHemlockVale where
  getAbilities (WelcomeToHemlockVale attrs) =
    [mkAbility attrs 1 $ forced $ DrawCard #when You (basic WeaknessCard) AnyDeck]

instance RunMessage WelcomeToHemlockVale where
  runMessage msg a@(WelcomeToHemlockVale attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator resign
      push R1
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> weakness) _ -> do
      cancelCardEffects attrs weakness
      discardCard iid attrs weakness
      pure a
    _ -> WelcomeToHemlockVale <$> liftRunMessage msg attrs
