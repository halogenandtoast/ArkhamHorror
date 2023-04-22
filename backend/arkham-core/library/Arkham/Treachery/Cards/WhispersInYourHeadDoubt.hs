module Arkham.Treachery.Cards.WhispersInYourHeadDoubt
  ( whispersInYourHeadDoubt
  , WhispersInYourHeadDoubt(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Matcher hiding (treacheryInHandOf)
import Arkham.Message
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WhispersInYourHeadDoubt = WhispersInYourHeadDoubt TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDoubt :: TreacheryCard WhispersInYourHeadDoubt
whispersInYourHeadDoubt =
  treachery WhispersInYourHeadDoubt Cards.whispersInYourHeadDoubt

instance HasModifiersFor WhispersInYourHeadDoubt where
  getModifiersFor (InvestigatorTarget iid) (WhispersInYourHeadDoubt a) =
    pure $ toModifiers
      a
      [ CannotPlay (CardWithType EventType) | treacheryInHandOf a == Just iid ]
  getModifiersFor _ _ = pure []

instance HasAbilities WhispersInYourHeadDoubt where
  getAbilities (WhispersInYourHeadDoubt a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility Nothing $ ActionCost 2]

instance RunMessage WhispersInYourHeadDoubt where
  runMessage msg t@(WhispersInYourHeadDoubt attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (addHiddenToHand iid attrs)
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      t <$ push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
    _ -> WhispersInYourHeadDoubt <$> runMessage msg attrs
