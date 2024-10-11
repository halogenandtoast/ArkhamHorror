module Arkham.Treachery.Cards.WhispersInYourHeadDismay (
  whispersInYourHeadDismay,
  WhispersInYourHeadDismay (..),
) where

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Matcher hiding (treacheryInHandOf)
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WhispersInYourHeadDismay = WhispersInYourHeadDismay TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDismay :: TreacheryCard WhispersInYourHeadDismay
whispersInYourHeadDismay = treachery WhispersInYourHeadDismay Cards.whispersInYourHeadDismay

instance HasModifiersFor WhispersInYourHeadDismay where
  getModifiersFor (InvestigatorTarget iid) (WhispersInYourHeadDismay a) =
    toModifiers a [CannotCommitCards $ CardWithType SkillType | treacheryInHandOf a == Just iid]
  getModifiersFor _ _ = pure []

instance HasAbilities WhispersInYourHeadDismay where
  getAbilities (WhispersInYourHeadDismay a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility [] $ ActionCost 2]

instance RunMessage WhispersInYourHeadDismay where
  runMessage msg t@(WhispersInYourHeadDismay attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WhispersInYourHeadDismay <$> runMessage msg attrs
