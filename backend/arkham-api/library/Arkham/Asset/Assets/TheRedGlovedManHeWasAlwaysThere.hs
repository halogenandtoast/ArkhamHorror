module Arkham.Asset.Assets.TheRedGlovedManHeWasAlwaysThere (theRedGlovedManHeWasAlwaysThere) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (canDiscoverCluesAtYourLocation)
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

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
                  [ WhileAttackingAnEnemy $ EnemyCanBeDamagedBySource (a.ability 1)
                  , WhileEvadingAnEnemy $ EnemyCanBeDamagedBySource (a.ability 1)
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
                  [ WhileAttackingAnEnemy AnyEnemy
                  , WhileEvadingAnEnemy AnyEnemy
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

      chooseOneM iid $ withI18n do
        for_ menemy \enemy -> do
          countVar 1
            $ labeled' "dealDamage"
            $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 enemy
        when discoverOk do
          countVar 1
            $ labeled' "discoverAtYourLocation"
            $ discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> TheRedGlovedManHeWasAlwaysThere <$> liftRunMessage msg attrs
