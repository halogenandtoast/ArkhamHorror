module Arkham.Asset.Assets.AlikiZoniUperetriaTheMaidWithTheScarletSash (alikiZoniUperetriaTheMaidWithTheScarletSash) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AlikiZoniUperetriaTheMaidWithTheScarletSash = AlikiZoniUperetriaTheMaidWithTheScarletSash AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alikiZoniUperetriaTheMaidWithTheScarletSash :: AssetCard AlikiZoniUperetriaTheMaidWithTheScarletSash
alikiZoniUperetriaTheMaidWithTheScarletSash =
  allyWith
    AlikiZoniUperetriaTheMaidWithTheScarletSash
    Cards.alikiZoniUperetriaTheMaidWithTheScarletSash
    (2, 2)
    noSlots

instance HasAbilities AlikiZoniUperetriaTheMaidWithTheScarletSash where
  getAbilities (AlikiZoniUperetriaTheMaidWithTheScarletSash a) =
    [ controlled
        a
        1
        (exists $ HollowedCard <> basic (not_ $ cardIs Cards.theRedGlovedManHeWasAlwaysThere))
        $ triggered
          (mapOneOf (CampaignEvent #after Nothing) ["exposed[enemy]", "exposed[decoy]", "exposed[location]"])
          (exhaust a)
    ]

instance RunMessage AlikiZoniUperetriaTheMaidWithTheScarletSash where
  runMessage msg a@(AlikiZoniUperetriaTheMaidWithTheScarletSash attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hollows <- select $ HollowedCard <> basic (not_ $ cardIs Cards.theRedGlovedManHeWasAlwaysThere)
      chooseTargetM iid hollows \c -> do
        case c.owner of
          Just owner -> drawCard owner c
          Nothing -> pure ()
      pure a
    _ -> AlikiZoniUperetriaTheMaidWithTheScarletSash <$> liftRunMessage msg attrs
