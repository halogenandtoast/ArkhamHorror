module Arkham.Treachery.Cards.GiftOfMadnessMisery (giftOfMadnessMisery, GiftOfMadnessMisery (..)) where

import Arkham.Ability
import Arkham.Choose
import Arkham.Classes
import Arkham.Matcher hiding (
  PlaceUnderneath,
  TreacheryInHandOf,
  treacheryInHandOf,
 )
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype GiftOfMadnessMisery = GiftOfMadnessMisery TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

giftOfMadnessMisery :: TreacheryCard GiftOfMadnessMisery
giftOfMadnessMisery = treachery GiftOfMadnessMisery Cards.giftOfMadnessMisery

instance HasModifiersFor GiftOfMadnessMisery where
  getModifiersFor (InvestigatorTarget iid) (GiftOfMadnessMisery a) =
    pure
      $ toModifiers
        a
        [ CannotTriggerAbilityMatching (AbilityIsActionAbility <> AbilityOnLocation Anywhere)
        | treacheryInHandOf a == Just iid
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities GiftOfMadnessMisery where
  getAbilities (GiftOfMadnessMisery a) =
    [restrictedAbility a 1 InYourHand actionAbility]

instance RunMessage GiftOfMadnessMisery where
  runMessage msg t@(GiftOfMadnessMisery attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ ChooseFrom iid $ chooseRandom attrs MonstersDeck 1
        , toDiscardBy iid (attrs.ability 1) attrs
        ]
      pure t
    ChoseCards _ chosen | isTarget attrs chosen.target -> do
      push $ PlaceUnderneath ActDeckTarget chosen.cards
      pure t
    _ -> GiftOfMadnessMisery <$> runMessage msg attrs
