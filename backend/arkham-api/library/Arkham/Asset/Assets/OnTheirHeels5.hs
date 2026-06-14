module Arkham.Asset.Assets.OnTheirHeels5 (onTheirHeels5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Taboo

newtype OnTheirHeels5 = OnTheirHeels5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheirHeels5 :: AssetCard OnTheirHeels5
onTheirHeels5 = asset OnTheirHeels5 Cards.onTheirHeels5

instance HasAbilities OnTheirHeels5 where
  getAbilities (OnTheirHeels5 a) =
    [ controlled
        a
        1
        -- "with 1 or more enemies" is the triggering condition, captured at entry
        -- by EntersLocationWithEnemy (#4813). The criteria below only gate that an
        -- effect is still possible at resolution (clue to discover or a damageable
        -- enemy), so the clue can still be discovered if the enemy was defeated
        -- during engagement.
        ( oneOf
            [ CanDiscoverCluesAt YourLocation
            , exists (EnemyAt YourLocation <> EnemyCanBeDamagedBySource (a.ability 1))
            ]
        )
        $ triggered
          (EntersLocationWithEnemy #after You)
          (assetUseCost a Lead 1 <> exhaust a)
    ]

instance RunMessage OnTheirHeels5 where
  runMessage msg a@(OnTheirHeels5 attrs) = runQueueT $ case msg of
    CardEnteredPlay _iid card | card.id == attrs.cardId -> do
      if tabooed TabooList25 attrs
        then pure $ OnTheirHeels5 $ attrs & whenNoUsesL ?~ DiscardWhenNoUses
        else pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        enemies <- select $ at_ (be lid) <> EnemyCanBeDamagedBySource (attrs.ability 1)
        mconcealed <- getConcealed (ForExpose $ toSource iid) iid
        chooseOrRunOneM iid do
          whenM (canDiscoverCluesAtYourLocation NotInvestigate iid) do
            withI18n
              $ countVar 1
              $ labeledI "discoverAtYourLocation"
              $ discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
          targets enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
          for_ mconcealed \card -> targeting card $ doFlip iid attrs card
      pure a
    _ -> OnTheirHeels5 <$> liftRunMessage msg attrs
