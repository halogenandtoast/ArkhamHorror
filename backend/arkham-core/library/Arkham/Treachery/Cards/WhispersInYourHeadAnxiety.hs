module Arkham.Treachery.Cards.WhispersInYourHeadAnxiety
  ( whispersInYourHeadAnxiety
  , WhispersInYourHeadAnxiety(..)
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

newtype WhispersInYourHeadAnxiety = WhispersInYourHeadAnxiety TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadAnxiety :: TreacheryCard WhispersInYourHeadAnxiety
whispersInYourHeadAnxiety =
  treachery WhispersInYourHeadAnxiety Cards.whispersInYourHeadAnxiety

instance HasModifiersFor WhispersInYourHeadAnxiety where
  getModifiersFor (InvestigatorHandTarget _) (WhispersInYourHeadAnxiety a) = pure $ toModifiers a [CannotTriggerFastAbilities]
  getModifiersFor _ _ = pure []

instance HasAbilities WhispersInYourHeadAnxiety where
  getAbilities (WhispersInYourHeadAnxiety a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility Nothing $ ActionCost 2]

instance RunMessage WhispersInYourHeadAnxiety where
  runMessage msg t@(WhispersInYourHeadAnxiety attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AddTreacheryToHand iid $ toId attrs)
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> WhispersInYourHeadAnxiety <$> runMessage msg attrs
