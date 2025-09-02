module Arkham.Enemy.Cards.TheContessaNeedlesslySmug (theContessaNeedlesslySmug) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype TheContessaNeedlesslySmug = TheContessaNeedlesslySmug EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theContessaNeedlesslySmug :: EnemyCard TheContessaNeedlesslySmug
theContessaNeedlesslySmug =
  enemy TheContessaNeedlesslySmug Cards.theContessaNeedlesslySmug (4, PerPlayer 4, 4) (1, 1)
    & setPrey MostRemainingHealth

instance HasAbilities TheContessaNeedlesslySmug where
  getAbilities (TheContessaNeedlesslySmug a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyEvaded #after You (be a)
      , restricted a 2 Never $ forced AnyWindow
      ]

instance RunMessage TheContessaNeedlesslySmug where
  runMessage msg e@(TheContessaNeedlesslySmug attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readyThis attrs
      roundModifier (attrs.ability 1) iid (CannotBeAttackedBy (be attrs))
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      throne <- selectJust $ locationIs Locations.throneOfBloodRedAsBloodBlackAsNight
      enemyMoveTo (attrs.ability 2) attrs throne
      pure e
    HandleElusive eid | eid == attrs.id -> do
      lead <- getLead
      whenMatch eid (not_ (EnemyAt $ locationIs Locations.throneOfBloodRedAsBloodBlackAsNight)) do
        chooseOneM lead $ abilityLabeled_ lead (mkAbility attrs 2 $ forced AnyWindow)
      pure e
    EnemyDamaged eid damageAssignment | eid == attrs.id -> do
      mcloak <- selectOne $ assetIs Assets.accursedCapeShroudOfChaos <> AssetAttachedTo (targetIs attrs)
      TheContessaNeedlesslySmug <$> case mcloak of
        Nothing -> liftRunMessage msg attrs
        Just cloak -> case damageAssignmentAmount damageAssignment of
          0 -> pure attrs
          1 -> do
            dealAssetDamage cloak (damageAssignmentSource damageAssignment) 1
            pure attrs
          n -> do
            result <-
              liftRunMessage (EnemyDamaged eid damageAssignment {damageAssignmentAmount = n - 1}) attrs
            dealAssetDamage cloak (damageAssignmentSource damageAssignment) 1
            pure result
    Flip _ _ (isTarget attrs -> True) -> do
      let enraged = lookupCard Cards.theContessaEnraged attrs.cardId
      push $ ReplaceEnemy attrs.id enraged Swap
      pure e
    _ -> TheContessaNeedlesslySmug <$> liftRunMessage msg attrs
