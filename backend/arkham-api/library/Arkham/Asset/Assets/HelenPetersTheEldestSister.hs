module Arkham.Asset.Assets.HelenPetersTheEldestSister (helenPetersTheEldestSister) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyEvaded)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (Time (..), getCampaignTime)
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose

newtype HelenPetersTheEldestSister = HelenPetersTheEldestSister AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

helenPetersTheEldestSister :: AssetCard HelenPetersTheEldestSister
helenPetersTheEldestSister = ally HelenPetersTheEldestSister Cards.helenPetersTheEldestSister (3, 2)

instance HasModifiersFor HelenPetersTheEldestSister where
  getModifiersFor (HelenPetersTheEldestSister a) = do
    time <- getCampaignTime
    controllerGets a $ case time of
      Day -> [SkillModifier #combat 1]
      Night -> [SkillModifier #agility 1]

instance HasAbilities HelenPetersTheEldestSister where
  getAbilities (HelenPetersTheEldestSister a) =
    [ controlled a 1 (exists $ EnemyAt YourLocation <> NonEliteEnemy)
        $ triggered
          (Enters #after You Anywhere)
          (exhaust a <> DamageCost (a.ability 1) (toTarget a) 1)
    ]

instance RunMessage HelenPetersTheEldestSister where
  runMessage msg a@(HelenPetersTheEldestSister attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt YourLocation <> NonEliteEnemy
      chooseTargetM iid enemies (automaticallyEvadeEnemy iid)
      pure a
    _ -> HelenPetersTheEldestSister <$> liftRunMessage msg attrs
