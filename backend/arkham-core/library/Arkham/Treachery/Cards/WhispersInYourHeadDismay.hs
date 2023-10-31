module Arkham.Treachery.Cards.WhispersInYourHeadDismay (
  whispersInYourHeadDismay,
  WhispersInYourHeadDismay (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Matcher hiding (treacheryInHandOf)
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WhispersInYourHeadDismay = WhispersInYourHeadDismay TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDismay :: TreacheryCard WhispersInYourHeadDismay
whispersInYourHeadDismay =
  treachery WhispersInYourHeadDismay Cards.whispersInYourHeadDismay

instance HasModifiersFor WhispersInYourHeadDismay where
  getModifiersFor (InvestigatorTarget iid) (WhispersInYourHeadDismay a) =
    pure
      $ toModifiers
        a
        [ CannotCommitCards $ CardWithType SkillType
        | treacheryInHandOf a == Just iid
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities WhispersInYourHeadDismay where
  getAbilities (WhispersInYourHeadDismay a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility Nothing $ ActionCost 2]

instance RunMessage WhispersInYourHeadDismay where
  runMessage msg t@(WhispersInYourHeadDismay attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (addHiddenToHand iid attrs)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      t <$ push (toDiscardBy iid (toAbilitySource attrs 1) $ toTarget attrs)
    _ -> WhispersInYourHeadDismay <$> runMessage msg attrs
