module Arkham.Treachery.Cards.LawOfYgirothChaos (
  lawOfYgirothChaos,
  LawOfYgirothChaos (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher hiding (TreacheryInHandOf, treacheryInHandOf)
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LawOfYgirothChaos = LawOfYgirothChaos TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lawOfYgirothChaos :: TreacheryCard LawOfYgirothChaos
lawOfYgirothChaos = treachery LawOfYgirothChaos Cards.lawOfYgirothChaos

instance HasModifiersFor LawOfYgirothChaos where
  getModifiersFor (InvestigatorTarget iid) (LawOfYgirothChaos a)
    | treacheryInHandOf a == Just iid =
        pure
          $ toModifiers
            a
            [CannotPlay CardWithOddCost, CannotTriggerAbilityMatching $ AbilityOnCard CardWithOddCost]
  getModifiersFor _ _ = pure []

instance HasAbilities LawOfYgirothChaos where
  getAbilities (LawOfYgirothChaos a) =
    [ restrictedAbility a 1 InYourHand
        $ actionAbilityWithCost (HandDiscardCost 1 CardWithEvenCost)
    ]

instance RunMessage LawOfYgirothChaos where
  runMessage msg t@(LawOfYgirothChaos attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> LawOfYgirothChaos <$> runMessage msg attrs
