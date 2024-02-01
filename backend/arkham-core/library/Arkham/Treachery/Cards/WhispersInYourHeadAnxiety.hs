module Arkham.Treachery.Cards.WhispersInYourHeadAnxiety (
  whispersInYourHeadAnxiety,
  WhispersInYourHeadAnxiety (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WhispersInYourHeadAnxiety = WhispersInYourHeadAnxiety TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

whispersInYourHeadAnxiety :: TreacheryCard WhispersInYourHeadAnxiety
whispersInYourHeadAnxiety =
  treachery WhispersInYourHeadAnxiety Cards.whispersInYourHeadAnxiety

instance HasModifiersFor WhispersInYourHeadAnxiety where
  getModifiersFor (InvestigatorTarget iid) (WhispersInYourHeadAnxiety a) =
    pure
      $ toModifiers
        a
        [CannotTriggerFastAbilities | treacheryInHandOf a == Just iid]
  getModifiersFor _ _ = pure []

instance HasAbilities WhispersInYourHeadAnxiety where
  getAbilities (WhispersInYourHeadAnxiety a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility [] $ ActionCost 2]

instance RunMessage WhispersInYourHeadAnxiety where
  runMessage msg t@(WhispersInYourHeadAnxiety attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> WhispersInYourHeadAnxiety <$> runMessage msg attrs
