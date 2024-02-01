module Arkham.Treachery.Cards.GiftOfMadnessMisery (
  giftOfMadnessMisery,
  GiftOfMadnessMisery (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher hiding (
  PlaceUnderneath,
  TreacheryInHandOf,
  treacheryInHandOf,
 )
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype GiftOfMadnessMisery = GiftOfMadnessMisery TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

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
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ DrawRandomFromScenarioDeck iid MonstersDeck (toTarget attrs) 1
        , toDiscardBy iid (toAbilitySource attrs 1) attrs
        ]
      pure t
    DrewFromScenarioDeck _ _ (isTarget attrs -> True) cards -> do
      push $ PlaceUnderneath ActDeckTarget cards
      pure t
    _ -> GiftOfMadnessMisery <$> runMessage msg attrs
