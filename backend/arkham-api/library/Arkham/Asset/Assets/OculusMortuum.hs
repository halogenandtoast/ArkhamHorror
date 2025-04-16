module Arkham.Asset.Assets.OculusMortuum (oculusMortuum) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks)
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Trait (Trait(Geist))

newtype OculusMortuum = OculusMortuum AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oculusMortuum :: AssetCard OculusMortuum
oculusMortuum = asset OculusMortuum Cards.oculusMortuum

instance HasAbilities OculusMortuum where
  getAbilities (OculusMortuum a) =
    [ restricted a 1 ControlsThis
        $ triggered
          ( oneOf
              [ CancelChaosToken #after You #any
              , IgnoreChaosToken #after You #any
              , ChaosTokenSealed #after You #any
              ]
          )
          (exhaust a)
    , controlled
        a
        2
        ( oneOf
            [ AbleToDiscoverCluesAt YourLocation
            , exists $ AttackingEnemy <> withTrait Geist <> EnemyCanBeDamagedBySource (a.ability 1)
            ]
        )
        $ triggered
          (EnemyAttacks #when (at_ YourLocation) AnyEnemyAttack AnyEnemy)
          (exhaust a <> assetUseCost a Evidence 1)
    ]

instance RunMessage OculusMortuum where
  runMessage msg a@(OculusMortuum attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      addUses (attrs.ability 1) attrs Evidence 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 2) 1
      mEnemy <- selectOne $ AttackingEnemy <> withTrait Geist <> EnemyCanBeDamagedBySource (attrs.ability 1)
      for_ mEnemy $ nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1 
      pure a
    _ -> OculusMortuum <$> liftRunMessage msg attrs
