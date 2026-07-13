module Arkham.Treachery.Cards.MilkOfShubNiggurathCircusExMortis (milkOfShubNiggurathCircusExMortis) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Elite))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MilkOfShubNiggurathCircusExMortis = MilkOfShubNiggurathCircusExMortis TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

milkOfShubNiggurathCircusExMortis :: TreacheryCard MilkOfShubNiggurathCircusExMortis
milkOfShubNiggurathCircusExMortis =
  treachery MilkOfShubNiggurathCircusExMortis Cards.milkOfShubNiggurathCircusExMortis

instance HasAbilities MilkOfShubNiggurathCircusExMortis where
  getAbilities (MilkOfShubNiggurathCircusExMortis x) = case x.attached.enemy of
    Just _ -> [mkAbility x 1 $ forced $ PhaseEnds #when #mythos]
    _ -> []

instance RunMessage MilkOfShubNiggurathCircusExMortis where
  runMessage msg t@(MilkOfShubNiggurathCircusExMortis attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <-
        select
          $ not_ (EnemyWithTrait Elite)
          <> not_ (EnemyWithAttachedTreachery $ treacheryIs Cards.milkOfShubNiggurathCircusExMortis)
      case enemies of
        [] -> gainSurge attrs
        xs -> chooseTargetM iid xs $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.attached.enemy \eid -> placeDoom (attrs.ability 1) eid 1
      pure t
    _ -> MilkOfShubNiggurathCircusExMortis <$> liftRunMessage msg attrs
