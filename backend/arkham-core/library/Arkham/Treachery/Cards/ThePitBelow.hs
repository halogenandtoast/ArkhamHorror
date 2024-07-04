module Arkham.Treachery.Cards.ThePitBelow (thePitBelow, ThePitBelow (..)) where

import Arkham.Ability
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ThePitBelow = ThePitBelow TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePitBelow :: TreacheryCard ThePitBelow
thePitBelow = treachery ThePitBelow Cards.thePitBelow

instance HasModifiersFor ThePitBelow where
  getModifiersFor (LocationTarget lid) (ThePitBelow attrs) =
    modified attrs [ShroudModifier 1 | treacheryOnLocation lid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities ThePitBelow where
  getAbilities (ThePitBelow a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage ThePitBelow where
  runMessage msg t@(ThePitBelow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        hasThePitBelow <- selectAny $ treacheryAt lid <> treacheryIs Cards.thePitBelow
        if hasThePitBelow then gainSurge attrs else attachTreachery attrs lid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ LocationWithTreachery (be attrs)
      for_ iids \iid -> assignDamage iid (attrs.ability 1) 3
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> ThePitBelow <$> liftRunMessage msg attrs
