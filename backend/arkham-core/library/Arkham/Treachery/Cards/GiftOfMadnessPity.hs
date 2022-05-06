module Arkham.Treachery.Cards.GiftOfMadnessPity
  ( giftOfMadnessPity
  , GiftOfMadnessPity(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Attrs
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype GiftOfMadnessPity = GiftOfMadnessPity TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

giftOfMadnessPity :: TreacheryCard GiftOfMadnessPity
giftOfMadnessPity = treachery GiftOfMadnessPity Cards.giftOfMadnessPity

instance HasModifiersFor env GiftOfMadnessPity where
  getModifiersFor _ (InvestigatorHandTarget _) (GiftOfMadnessPity a) = pure $ toModifiers a [CannotFight (EnemyWithTrait Lunatic)]
  getModifiersFor _ _ _ = pure []

instance HasAbilities GiftOfMadnessPity where
  getAbilities (GiftOfMadnessPity a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility Nothing $ ActionCost 1]

instance TreacheryRunner env => RunMessage env GiftOfMadnessPity where
  runMessage msg t@(GiftOfMadnessPity attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AddTreacheryToHand iid $ toId attrs)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> t <$ pushAll
      [ DrawRandomFromScenarioDeck iid MonstersDeck (toTarget attrs) 1
      , Discard $ toTarget attrs
      ]
    DrewFromScenarioDeck _ _ target cards | isTarget attrs target ->
      t <$ push (PlaceUnderneath ActDeckTarget cards)
    _ -> GiftOfMadnessPity <$> runMessage msg attrs
