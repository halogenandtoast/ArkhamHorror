module Arkham.Treachery.Cards.TheHarbinger
  ( theHarbinger
  , TheHarbinger(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Deck
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype TheHarbinger = TheHarbinger TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHarbinger :: TreacheryCard TheHarbinger
theHarbinger = treachery TheHarbinger Cards.theHarbinger

instance HasModifiersFor TheHarbinger where
  getModifiersFor target (TheHarbinger a)
    | Just target == treacheryAttachedTarget a = pure
    $ toModifiers a [CannotManipulateDeck]
  getModifiersFor _ _ = pure []

instance HasAbilities TheHarbinger where
  getAbilities (TheHarbinger a) = case treacheryAttachedTarget a of
    Just (InvestigatorTarget iid) ->
      [ restrictedAbility
            a
            1
            (InvestigatorExists $ You <> InvestigatorWithId iid)
          $ ActionAbility Nothing
          $ ActionCost 2
      ]
    _ -> []

instance RunMessage TheHarbinger where
  runMessage msg t@(TheHarbinger attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      ([AttachTreachery (toId attrs) (InvestigatorTarget iid)]
      <> [ PutCardOnTopOfDeck iid (InvestigatorDeck iid) (toCard c)
         | c <- maybeToList . toPlayerCard $ toCard attrs
         ]
      )
    _ -> TheHarbinger <$> runMessage msg attrs
