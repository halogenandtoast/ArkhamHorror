module Arkham.Types.Treachery.Cards.WhispersInYourHeadDread
  ( whispersInYourHeadDread
  , WhispersInYourHeadDread(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype WhispersInYourHeadDread = WhispersInYourHeadDread TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDread :: TreacheryCard WhispersInYourHeadDread
whispersInYourHeadDread =
  treachery WhispersInYourHeadDread Cards.whispersInYourHeadDread

instance HasModifiersFor env WhispersInYourHeadDread where
  getModifiersFor _ (InvestigatorTarget iid) (WhispersInYourHeadDread a)
    | Just iid == treacheryInHandOf a = pure
    $ toModifiers a [CannotMoveMoreThanOnceEachTurn]
  getModifiersFor _ _ _ = pure []

instance HasAbilities WhispersInYourHeadDread where
  getAbilities (WhispersInYourHeadDread a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility Nothing $ ActionCost 2]

instance TreacheryRunner env => RunMessage env WhispersInYourHeadDread where
  runMessage msg t@(WhispersInYourHeadDread attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AddTreacheryToHand iid $ toId attrs)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> WhispersInYourHeadDread <$> runMessage msg attrs
