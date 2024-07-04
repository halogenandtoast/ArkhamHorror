module Arkham.Treachery.Cards.LawOfYgirothDiscord (lawOfYgirothDiscord, LawOfYgirothDiscord (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (TreacheryInHandOf, treacheryInHandOf)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LawOfYgirothDiscord = LawOfYgirothDiscord TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lawOfYgirothDiscord :: TreacheryCard LawOfYgirothDiscord
lawOfYgirothDiscord = treachery LawOfYgirothDiscord Cards.lawOfYgirothDiscord

instance HasModifiersFor LawOfYgirothDiscord where
  getModifiersFor (InvestigatorTarget iid) (LawOfYgirothDiscord a) | treacheryInHandOf a == Just iid = do
    modified a [CannotCommitCards CardWithOddSkillIcons]
  getModifiersFor _ _ = pure []

instance HasAbilities LawOfYgirothDiscord where
  getAbilities (LawOfYgirothDiscord a) =
    [ restrictedAbility a 1 InYourHand $ actionAbilityWithCost (HandDiscardCost 1 CardWithEvenSkillIcons)
    ]

instance RunMessage LawOfYgirothDiscord where
  runMessage msg t@(LawOfYgirothDiscord attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> LawOfYgirothDiscord <$> liftRunMessage msg attrs
