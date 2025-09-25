module Arkham.Treachery.Cards.ImpendingEvils (impendingEvils) where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ImpendingEvils = ImpendingEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

impendingEvils :: TreacheryCard ImpendingEvils
impendingEvils = treachery ImpendingEvils Cards.impendingEvils

instance RunMessage ImpendingEvils where
  runMessage msg t@(ImpendingEvils attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid do
        withI18n $ numberVar "damage" 1
          $ numberVar "horror" 1
          $ labeled' "takeDamageAndHorror"
          $ assignDamageAndHorror iid (attrs.ability 1) 1 1
        campaignI18n $ labeled' "impendingEvils.place" do
          gainSurge attrs
          placeTreachery attrs NextToAgenda
      pure t
    AfterRevelation _ tid | tid == toId attrs -> do
      copies <- select $ treacheryIs Cards.impendingEvils
      when (length copies >= 3) do
        for_ copies $ toDiscard attrs
        advanceCurrentAgenda attrs
      pure t
    _ -> ImpendingEvils <$> liftRunMessage msg attrs
