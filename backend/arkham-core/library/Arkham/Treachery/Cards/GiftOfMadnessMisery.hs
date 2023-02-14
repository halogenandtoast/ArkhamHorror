module Arkham.Treachery.Cards.GiftOfMadnessMisery
  ( giftOfMadnessMisery
  , GiftOfMadnessMisery(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding ( PlaceUnderneath, treacheryInHandOf, TreacheryInHandOf )
import Arkham.Message
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype GiftOfMadnessMisery = GiftOfMadnessMisery TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

giftOfMadnessMisery :: TreacheryCard GiftOfMadnessMisery
giftOfMadnessMisery = treachery GiftOfMadnessMisery Cards.giftOfMadnessMisery

instance HasModifiersFor GiftOfMadnessMisery where
  getModifiersFor (InvestigatorTarget iid) (GiftOfMadnessMisery a) =
    pure $ toModifiers
      a
      [ CannotFight (EnemyWithTrait Lunatic) | treacheryInHandOf a == Just iid ]
  getModifiersFor _ _ = pure []

instance HasAbilities GiftOfMadnessMisery where
  getAbilities (GiftOfMadnessMisery a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage GiftOfMadnessMisery where
  runMessage msg t@(GiftOfMadnessMisery attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (addHiddenToHand iid attrs)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> t <$ pushAll
      [ DrawRandomFromScenarioDeck iid MonstersDeck (toTarget attrs) 1
      , Discard (toAbilitySource attrs 1) (toTarget attrs)
      ]
    DrewFromScenarioDeck _ _ target cards | isTarget attrs target ->
      t <$ push (PlaceUnderneath ActDeckTarget cards)
    _ -> GiftOfMadnessMisery <$> runMessage msg attrs
