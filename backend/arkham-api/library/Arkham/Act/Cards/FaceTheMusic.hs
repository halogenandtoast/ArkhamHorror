module Arkham.Act.Cards.FaceTheMusic (faceTheMusic) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Criminal, Elite))

newtype FaceTheMusic = FaceTheMusic ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

faceTheMusic :: ActCard FaceTheMusic
faceTheMusic = act (3, A) FaceTheMusic Cards.faceTheMusic Nothing

instance HasModifiersFor FaceTheMusic where
  getModifiersFor (FaceTheMusic a) = modifySelect a (withTrait Criminal <> EnemyWithAnyClues) [#aloof]

instance HasAbilities FaceTheMusic where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ parleyAction
          (SpendTokenCost #clue $ EnemyTargetMatches $ EnemyWithTrait Criminal <> at_ YourLocation)
    , restricted a 2 (InVictoryDisplay (CardWithTrait Elite) (atLeast 1))
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage FaceTheMusic where
  runMessage msg a@(FaceTheMusic attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (paymentTargets -> xs) -> do
      for_ (mapMaybe (.enemy) xs) \enemy -> do
        dmg <- field EnemyHealthDamage enemy
        enemies <-
          select
            $ at_ (locationWithInvestigator iid)
            <> not_ (EnemyWithId enemy)
            <> EnemyCanBeDamagedBySource (attrs.ability 1)
        chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) dmg
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> FaceTheMusic <$> liftRunMessage msg attrs
