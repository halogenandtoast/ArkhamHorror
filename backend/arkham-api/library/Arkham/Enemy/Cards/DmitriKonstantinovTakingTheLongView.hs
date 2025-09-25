module Arkham.Enemy.Cards.DmitriKonstantinovTakingTheLongView (dmitriKonstantinovTakingTheLongView) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Helpers.Window (getPassedBy)
import Arkham.Trait (Trait (SilverTwilight))

newtype DmitriKonstantinovTakingTheLongView = DmitriKonstantinovTakingTheLongView EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dmitriKonstantinovTakingTheLongView :: EnemyCard DmitriKonstantinovTakingTheLongView
dmitriKonstantinovTakingTheLongView =
  enemy
    DmitriKonstantinovTakingTheLongView
    Cards.dmitriKonstantinovTakingTheLongView
    (4, Static 3, 2)
    (1, 0)
    & setSpawnAt
      (FarthestLocationFromInvestigator You $ not_ (LocationWithEnemy $ EnemyWithTrait SilverTwilight))

instance HasAbilities DmitriKonstantinovTakingTheLongView where
  getAbilities (DmitriKonstantinovTakingTheLongView a) = extend a
    [ restricted a 1 CanPlaceDoomOnThis $ forced $ EnemySpawns #after Anywhere (be a)
    , restricted a 2 (thisExists a EnemyWithAnyDoom) $ freeReaction $ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) (SuccessResult $ atLeast 2)
    ]

instance RunMessage DmitriKonstantinovTakingTheLongView where
  runMessage msg e@(DmitriKonstantinovTakingTheLongView attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 2
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 (getPassedBy -> n) _ -> do
      removeDoom (attrs.ability 2) attrs (n `div` 2)
      pure e
    _ -> DmitriKonstantinovTakingTheLongView <$> liftRunMessage msg attrs
