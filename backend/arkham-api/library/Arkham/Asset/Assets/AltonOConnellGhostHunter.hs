module Arkham.Asset.Assets.AltonOConnellGhostHunter (altonOConnellGhostHunter) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Token

newtype AltonOConnellGhostHunter = AltonOConnellGhostHunter AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

altonOConnellGhostHunter :: AssetCard AltonOConnellGhostHunter
altonOConnellGhostHunter = ally AltonOConnellGhostHunter Cards.altonOConnellGhostHunter (2, 1)

instance HasModifiersFor AltonOConnellGhostHunter where
  getModifiersFor (AltonOConnellGhostHunter a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities AltonOConnellGhostHunter where
  getAbilities (AltonOConnellGhostHunter a) =
    [ restricted a 1 ControlsThis
        $ freeReaction
        $ SkillTestResult #after You #any (SuccessResult $ static 3)
    ]
      <> [ controlled a 2 (AbleToDiscoverCluesAt YourLocation)
             $ FastAbility
             $ exhaust a
             <> dynamicAssetUseCost
               a
               Evidence
               (InvestigatorLocationMaybeFieldCalculation controller LocationShroud)
         | controller <- maybeToList a.controller
         ]

instance RunMessage AltonOConnellGhostHunter where
  runMessage msg a@(AltonOConnellGhostHunter attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Evidence 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 2) 1
      pure a
    _ -> AltonOConnellGhostHunter <$> liftRunMessage msg attrs
