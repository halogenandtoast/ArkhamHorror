module Arkham.Treachery.Cards.WhispersInYourHeadDread (
  whispersInYourHeadDread,
  WhispersInYourHeadDread (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WhispersInYourHeadDread = WhispersInYourHeadDread TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDread :: TreacheryCard WhispersInYourHeadDread
whispersInYourHeadDread =
  treachery WhispersInYourHeadDread Cards.whispersInYourHeadDread

instance HasModifiersFor WhispersInYourHeadDread where
  getModifiersFor (WhispersInYourHeadDread a) = case a.placement of
    HiddenInHand iid -> modified_ a iid [CannotMoveMoreThanOnceEachTurn]
    _ -> pure mempty

instance HasAbilities WhispersInYourHeadDread where
  getAbilities (WhispersInYourHeadDread a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility [] $ ActionCost 2]

instance RunMessage WhispersInYourHeadDread where
  runMessage msg t@(WhispersInYourHeadDread attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (addHiddenToHand iid attrs)
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) ->
      t <$ push (toDiscardBy iid (toAbilitySource attrs 1) attrs)
    _ -> WhispersInYourHeadDread <$> runMessage msg attrs
