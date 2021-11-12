module Arkham.Types.Treachery.Cards.GiftOfMadnessMisery
  ( giftOfMadnessMisery
  , GiftOfMadnessMisery(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher hiding (PlaceUnderneath)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Scenario.Deck
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype GiftOfMadnessMisery = GiftOfMadnessMisery TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

giftOfMadnessMisery :: TreacheryCard GiftOfMadnessMisery
giftOfMadnessMisery = treachery GiftOfMadnessMisery Cards.giftOfMadnessMisery

instance HasModifiersFor env GiftOfMadnessMisery where
  getModifiersFor _ (InvestigatorTarget iid) (GiftOfMadnessMisery a)
    | Just iid == treacheryInHandOf a = pure
    $ toModifiers a [CannotFight (EnemyWithTrait Lunatic)]
  getModifiersFor _ _ _ = pure []

instance HasAbilities GiftOfMadnessMisery where
  getAbilities (GiftOfMadnessMisery a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility Nothing $ ActionCost 1]

instance TreacheryRunner env => RunMessage env GiftOfMadnessMisery where
  runMessage msg t@(GiftOfMadnessMisery attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AddTreacheryToHand iid $ toId attrs)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> t <$ pushAll
      [ DrawRandomFromScenarioDeck iid MonstersDeck (toTarget attrs) 1
      , Discard $ toTarget attrs
      ]
    DrewFromScenarioDeck _ _ target cards | isTarget attrs target ->
      t <$ push (PlaceUnderneath ActDeckTarget cards)
    _ -> GiftOfMadnessMisery <$> runMessage msg attrs
