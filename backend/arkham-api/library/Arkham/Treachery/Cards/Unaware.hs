module Arkham.Treachery.Cards.Unaware (unaware) where

import Arkham.Ability
import Arkham.Card
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Unaware = Unaware TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unaware :: TreacheryCard Unaware
unaware = treachery Unaware Cards.unaware

instance HasAbilities Unaware where
  getAbilities (Unaware attrs) =
    [ playerLimit PerTurn
        $ restrictedAbility attrs 1 (InThreatAreaOf You)
        $ forced
        $ SkillTestResult #after You AnySkillTest #failure
    ]

instance RunMessage Unaware where
  runMessage msg t@(Unaware attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCardsEdit iid attrs 1 (setTarget attrs)
      pure t
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      for_ drewCards.cards \card -> do
        when (toCardType card == EnemyType) do
          toDiscardBy iid (attrs.ability 1) attrs
        drawCardFrom iid drewCards.deck card
      pure t
    _ -> Unaware <$> liftRunMessage msg attrs
