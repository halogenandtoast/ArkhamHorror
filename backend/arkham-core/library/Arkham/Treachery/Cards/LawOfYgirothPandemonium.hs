module Arkham.Treachery.Cards.LawOfYgirothPandemonium (
  lawOfYgirothPandemonium,
  LawOfYgirothPandemonium (..),
)
where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (TreacheryInHandOf, treacheryInHandOf)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LawOfYgirothPandemonium = LawOfYgirothPandemonium TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lawOfYgirothPandemonium :: TreacheryCard LawOfYgirothPandemonium
lawOfYgirothPandemonium = treachery LawOfYgirothPandemonium Cards.lawOfYgirothPandemonium

instance HasModifiersFor LawOfYgirothPandemonium where
  getModifiersFor (InvestigatorTarget iid) (LawOfYgirothPandemonium a) | treacheryInHandOf a == Just iid = do
    modified
      a
      [CannotPlay CardWithOddNumberOfWordsInTitle, CannotCommitCards CardWithOddNumberOfWordsInTitle]
  getModifiersFor _ _ = pure []

instance HasAbilities LawOfYgirothPandemonium where
  getAbilities (LawOfYgirothPandemonium a) =
    [ restrictedAbility a 1 InYourHand
        $ actionAbilityWithCost (HandDiscardCost 1 CardWithEvenNumberOfWordsInTitle)
    ]

instance RunMessage LawOfYgirothPandemonium where
  runMessage msg t@(LawOfYgirothPandemonium attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> LawOfYgirothPandemonium <$> liftRunMessage msg attrs
