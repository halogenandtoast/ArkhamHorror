module Arkham.Treachery.Cards.EndOfNegotiations (endOfNegotiations) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Criminal))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (PerformAction)

newtype EndOfNegotiations = EndOfNegotiations TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endOfNegotiations :: TreacheryCard EndOfNegotiations
endOfNegotiations = treachery EndOfNegotiations Cards.endOfNegotiations

instance HasAbilities EndOfNegotiations where
  getAbilities (EndOfNegotiations attrs) = case attrs.attached of
    Just (EnemyTarget eid) ->
      [ mkAbility attrs 1
          $ forced
          $ PerformAction #after (You <> InvestigatorAt (LocationWithEnemy $ EnemyWithId eid)) #parley
      , skillTestAbility $ restrictedAbility attrs 2 OnSameLocation actionAbility
      ]
    _ -> [skillTestAbility $ restrictedAbility attrs 2 OnSameLocation actionAbility]

instance RunMessage EndOfNegotiations where
  runMessage msg t@(EndOfNegotiations attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <-
        select
          $ NearestEnemyTo iid
          $ EnemyWithTrait Criminal
          <> not_ (EnemyWithAttachedTreachery $ treacheryIs Cards.endOfNegotiations)
      if null enemies
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets enemies $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached \case
        EnemyTarget eid -> initiateEnemyAttack eid attrs iid
        _ -> pure ()
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> EndOfNegotiations <$> liftRunMessage msg attrs
