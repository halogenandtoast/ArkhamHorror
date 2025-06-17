module Arkham.Enemy.Cards.ValeriyaAntonovaDontMessWithHer (valeriyaAntonovaDontMessWithHer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.SkillType (allSkills)
import Arkham.Trait (Trait (Guest))

newtype ValeriyaAntonovaDontMessWithHer = ValeriyaAntonovaDontMessWithHer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeriyaAntonovaDontMessWithHer :: EnemyCard ValeriyaAntonovaDontMessWithHer
valeriyaAntonovaDontMessWithHer =
  enemy ValeriyaAntonovaDontMessWithHer Cards.valeriyaAntonovaDontMessWithHer (2, Static 3, 3) (0, 1)

instance HasModifiersFor ValeriyaAntonovaDontMessWithHer where
  getModifiersFor (ValeriyaAntonovaDontMessWithHer a) = do
    modifySelf a [CannotBeDamaged]
    modifySelectMaybe a (InvestigatorAt $ locationWithEnemy a) \iid -> do
      hasGuest <-
        lift
          $ selectAny
          $ AssetControlledBy (InvestigatorWithId iid)
          <> AssetWithTrait Guest
          <> AssetExhausted
      guard hasGuest
      pure [SkillModifier s (-1) | s <- allSkills]

instance HasAbilities ValeriyaAntonovaDontMessWithHer where
  getAbilities (ValeriyaAntonovaDontMessWithHer a) =
    [ mkAbility a 1 $ forced $ RoundEnds #when
    ]

instance RunMessage ValeriyaAntonovaDontMessWithHer where
  runMessage msg e@(ValeriyaAntonovaDontMessWithHer attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      assets <- select $ AssetWithTrait Guest <> at_ (locationWithEnemy attrs)
      pushAll [Exhaust (toTarget aid) | aid <- assets]
      pure e
    _ -> ValeriyaAntonovaDontMessWithHer <$> liftRunMessage msg attrs
