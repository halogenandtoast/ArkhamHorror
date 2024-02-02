module Arkham.Treachery.Cards.LawOfYgirothPandemonium (
  lawOfYgirothPandemonium,
  LawOfYgirothPandemonium (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher hiding (TreacheryInHandOf, treacheryInHandOf)
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LawOfYgirothPandemonium = LawOfYgirothPandemonium TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lawOfYgirothPandemonium :: TreacheryCard LawOfYgirothPandemonium
lawOfYgirothPandemonium = treachery LawOfYgirothPandemonium Cards.lawOfYgirothPandemonium

instance HasModifiersFor LawOfYgirothPandemonium where
  getModifiersFor (InvestigatorTarget iid) (LawOfYgirothPandemonium a)
    | treacheryInHandOf a == Just iid =
        pure
          $ toModifiers
            a
            [CannotPlay CardWithOddNumberOfWordsInTitle, CannotCommitCards CardWithOddNumberOfWordsInTitle]
  getModifiersFor _ _ = pure []

instance HasAbilities LawOfYgirothPandemonium where
  getAbilities (LawOfYgirothPandemonium a) =
    [ restrictedAbility a 1 InYourHand
        $ actionAbilityWithCost (HandDiscardCost 1 CardWithEvenNumberOfWordsInTitle)
    ]

instance RunMessage LawOfYgirothPandemonium where
  runMessage msg t@(LawOfYgirothPandemonium attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> LawOfYgirothPandemonium <$> runMessage msg attrs
