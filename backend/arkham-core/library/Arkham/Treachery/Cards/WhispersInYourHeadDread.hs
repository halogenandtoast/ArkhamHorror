module Arkham.Treachery.Cards.WhispersInYourHeadDread
  ( whispersInYourHeadDread
  , WhispersInYourHeadDread(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WhispersInYourHeadDread = WhispersInYourHeadDread TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDread :: TreacheryCard WhispersInYourHeadDread
whispersInYourHeadDread =
  treachery WhispersInYourHeadDread Cards.whispersInYourHeadDread

instance HasModifiersFor WhispersInYourHeadDread where
  getModifiersFor (InvestigatorHandTarget _) (WhispersInYourHeadDread a) = pure $ toModifiers a [CannotMoveMoreThanOnceEachTurn]
  getModifiersFor _ _ = pure []

instance HasAbilities WhispersInYourHeadDread where
  getAbilities (WhispersInYourHeadDread a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility Nothing $ ActionCost 2]

instance RunMessage WhispersInYourHeadDread where
  runMessage msg t@(WhispersInYourHeadDread attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AddTreacheryToHand iid $ toId attrs)
    InHand _ (UseCardAbility _ (isSource attrs -> True) _ 1 _) ->
      t <$ push (Discard $ toTarget attrs)
    _ -> WhispersInYourHeadDread <$> runMessage msg attrs
