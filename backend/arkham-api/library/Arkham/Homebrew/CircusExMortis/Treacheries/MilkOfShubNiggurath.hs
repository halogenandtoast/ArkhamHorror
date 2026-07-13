module Arkham.Homebrew.CircusExMortis.Treacheries.MilkOfShubNiggurath (milkOfShubNiggurath) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Elite))
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MilkOfShubNiggurath = MilkOfShubNiggurath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

milkOfShubNiggurath :: TreacheryCard MilkOfShubNiggurath
milkOfShubNiggurath =
  treachery MilkOfShubNiggurath Cards.milkOfShubNiggurath

instance HasAbilities MilkOfShubNiggurath where
  getAbilities (MilkOfShubNiggurath x) = case x.attached.enemy of
    Just _ -> [mkAbility x 1 $ forced $ PhaseEnds #when #mythos]
    _ -> []

instance RunMessage MilkOfShubNiggurath where
  runMessage msg t@(MilkOfShubNiggurath attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <-
        select
          $ not_ (EnemyWithTrait Elite)
          <> not_ (EnemyWithAttachedTreachery $ treacheryIs Cards.milkOfShubNiggurath)
      case enemies of
        [] -> gainSurge attrs
        xs -> chooseTargetM iid xs $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.attached.enemy \eid -> placeDoom (attrs.ability 1) eid 1
      pure t
    _ -> MilkOfShubNiggurath <$> liftRunMessage msg attrs
