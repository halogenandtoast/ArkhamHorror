module Arkham.Treachery.Cards.LawOfYgirothDiscord (
  lawOfYgirothDiscord,
  LawOfYgirothDiscord (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher hiding (TreacheryInHandOf, treacheryInHandOf)
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LawOfYgirothDiscord = LawOfYgirothDiscord TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lawOfYgirothDiscord :: TreacheryCard LawOfYgirothDiscord
lawOfYgirothDiscord = treachery LawOfYgirothDiscord Cards.lawOfYgirothDiscord

instance HasModifiersFor LawOfYgirothDiscord where
  getModifiersFor (InvestigatorTarget iid) (LawOfYgirothDiscord a)
    | treacheryInHandOf a == Just iid =
        pure $ toModifiers a [CannotCommitCards CardWithOddSkillIcons]
  getModifiersFor _ _ = pure []

instance HasAbilities LawOfYgirothDiscord where
  getAbilities (LawOfYgirothDiscord a) =
    [ restrictedAbility a 1 InYourHand
        $ actionAbilityWithCost (HandDiscardCost 1 CardWithEvenSkillIcons)
    ]

instance RunMessage LawOfYgirothDiscord where
  runMessage msg t@(LawOfYgirothDiscord attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> LawOfYgirothDiscord <$> runMessage msg attrs
