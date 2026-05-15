module Arkham.Asset.Assets.TheRedGlovedManHeWasAlwaysThere (theRedGlovedManHeWasAlwaysThere) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (canDiscoverCluesAtYourLocation)
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Trait (Trait (Outsider))

newtype TheRedGlovedManHeWasAlwaysThere = TheRedGlovedManHeWasAlwaysThere AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedGlovedManHeWasAlwaysThere :: AssetCard TheRedGlovedManHeWasAlwaysThere
theRedGlovedManHeWasAlwaysThere = allyWith TheRedGlovedManHeWasAlwaysThere Cards.theRedGlovedManHeWasAlwaysThere (3, 3) noSlots

instance HasAbilities TheRedGlovedManHeWasAlwaysThere where
  getAbilities (TheRedGlovedManHeWasAlwaysThere a) =
    [ controlled_ a 1
        $ triggered
          ( SkillTestResult
              #after
              You
              ( oneOf
                  [ WhileAttackingAnEnemy $ EnemyWithTrait Outsider <> EnemyCanBeDamagedBySource (a.ability 1)
                  , WhileEvadingAnEnemy $ EnemyWithTrait Outsider <> EnemyCanBeDamagedBySource (a.ability 1)
                  ]
              )
              (SuccessResult $ atLeast 2)
          )
          (exhaust a)
    , controlled a 1 (CanDiscoverCluesAt YourLocation)
        $ triggered
          ( SkillTestResult
              #after
              You
              ( oneOf
                  [ WhileAttackingAnEnemy (EnemyWithTrait Outsider)
                  , WhileEvadingAnEnemy (EnemyWithTrait Outsider)
                  ]
              )
              (SuccessResult $ atLeast 2)
          )
          (exhaust a)
    ]

instance RunMessage TheRedGlovedManHeWasAlwaysThere where
  runMessage msg a@(TheRedGlovedManHeWasAlwaysThere attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      menemy <- getSkillTestTargetedEnemy
      discoverOk <- canDiscoverCluesAtYourLocation NotInvestigate iid
      for_ menemy $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
      when discoverOk
        $ discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> TheRedGlovedManHeWasAlwaysThere <$> liftRunMessage msg attrs
