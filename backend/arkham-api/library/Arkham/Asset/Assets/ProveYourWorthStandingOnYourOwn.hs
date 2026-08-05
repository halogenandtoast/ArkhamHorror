module Arkham.Asset.Assets.ProveYourWorthStandingOnYourOwn (proveYourWorthCompleted) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher

newtype ProveYourWorthStandingOnYourOwn = ProveYourWorthStandingOnYourOwn AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

proveYourWorthCompleted :: AssetCard ProveYourWorthStandingOnYourOwn
proveYourWorthCompleted = asset ProveYourWorthStandingOnYourOwn Cards.proveYourWorthCompleted

instance HasAbilities ProveYourWorthStandingOnYourOwn where
  getAbilities (ProveYourWorthStandingOnYourOwn a) =
    [ controlled a 1 (DuringSkillTest AnySkillTest)
        $ triggered
          (RevealChaosToken #cancel (affectsColocatedMatch You) (not_ #autofail))
          (assetUseCost a #chance 1)
    ]

instance RunMessage ProveYourWorthStandingOnYourOwn where
  runMessage msg a@(ProveYourWorthStandingOnYourOwn attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      -- "Cancel that token" and nothing else: no replacement token is drawn.
      cancelChaosToken (attrs.ability 1) iid token
      pure a
    _ -> ProveYourWorthStandingOnYourOwn <$> liftRunMessage msg attrs
