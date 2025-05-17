module Arkham.Treachery.Cards.TheHarbinger (theHarbinger) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheHarbinger = TheHarbinger TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHarbinger :: TreacheryCard TheHarbinger
theHarbinger = treachery TheHarbinger Cards.theHarbinger

instance HasModifiersFor TheHarbinger where
  getModifiersFor (TheHarbinger a) = case a.placement of
    OnTopOfDeck iid -> modified_ a iid [CannotManipulateDeck]
    _ -> pure mempty

instance HasAbilities TheHarbinger where
  getAbilities (TheHarbinger a) = case treacheryOnTopOfDeck a of
    Just iid -> [restricted a 1 (youExist $ at_ $ locationWithInvestigator iid) doubleActionAbility]
    _ -> []

instance RunMessage TheHarbinger where
  runMessage msg t@(TheHarbinger attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ RemoveTreachery attrs.id
      for_ (toPlayerCard attrs) (putCardOnTopOfDeck iid iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ (toPlayerCard attrs) obtainCard
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> TheHarbinger <$> liftRunMessage msg attrs
