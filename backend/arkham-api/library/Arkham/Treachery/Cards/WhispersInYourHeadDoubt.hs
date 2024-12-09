module Arkham.Treachery.Cards.WhispersInYourHeadDoubt (
  whispersInYourHeadDoubt,
  WhispersInYourHeadDoubt (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Matcher hiding (treacheryInHandOf)
import Arkham.Modifier
import Arkham.Placement
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
  getModifiersFor (WhispersInYourHeadDoubt a) = case a.placement of
    HiddenInHand iid -> modified_ a iid [CannotPlay (CardWithType EventType)]
    _ -> pure mempty

instance HasAbilities WhispersInYourHeadDoubt where
  getAbilities (WhispersInYourHeadDoubt a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility [] $ ActionCost 2]

instance RunMessage WhispersInYourHeadDoubt where
  runMessage msg t@(WhispersInYourHeadDoubt attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ addHiddenToHand iid attrs
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> WhispersInYourHeadDoubt <$> runMessage msg attrs
