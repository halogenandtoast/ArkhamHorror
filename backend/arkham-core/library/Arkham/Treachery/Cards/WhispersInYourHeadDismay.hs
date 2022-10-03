module Arkham.Treachery.Cards.WhispersInYourHeadDismay
  ( whispersInYourHeadDismay
  , WhispersInYourHeadDismay(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WhispersInYourHeadDismay = WhispersInYourHeadDismay TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDismay :: TreacheryCard WhispersInYourHeadDismay
whispersInYourHeadDismay =
  treachery WhispersInYourHeadDismay Cards.whispersInYourHeadDismay

instance HasModifiersFor WhispersInYourHeadDismay where
  getModifiersFor (InvestigatorHandTarget _) (WhispersInYourHeadDismay a) = pure $ toModifiers a [CannotCommitCards $ CardWithType SkillType]
  getModifiersFor _ _ = pure []

instance HasAbilities WhispersInYourHeadDismay where
  getAbilities (WhispersInYourHeadDismay a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility Nothing $ ActionCost 2]

instance RunMessage WhispersInYourHeadDismay where
  runMessage msg t@(WhispersInYourHeadDismay attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AddTreacheryToHand iid $ toId attrs)
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> WhispersInYourHeadDismay <$> runMessage msg attrs
