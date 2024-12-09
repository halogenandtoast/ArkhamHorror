module Arkham.Treachery.Cards.TheHarbinger (theHarbinger, TheHarbinger (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

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
    Just iid ->
      [ restricted a 1 (youExist $ InvestigatorAt $ locationWithInvestigator iid)
          $ ActionAbility [] (ActionCost 2)
      ]
    _ -> []

instance RunMessage TheHarbinger where
  runMessage msg t@(TheHarbinger attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      pushAll
        $ [PlaceTreachery (toId attrs) (OnTopOfDeck iid)]
        <> [ PutCardOnTopOfDeck iid (InvestigatorDeck iid) (toCard c)
           | c <- maybeToList . toPlayerCard $ toCard attrs
           ]
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [ObtainCard (toCard attrs).id, toDiscardBy iid attrs attrs]
      pure t
    _ -> TheHarbinger <$> runMessage msg attrs
