module Arkham.Asset.Assets.UnscrupulousLoan3 (unscrupulousLoan3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Investigator (eliminationWindow)
import Arkham.Matcher

newtype UnscrupulousLoan3 = UnscrupulousLoan3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unscrupulousLoan3 :: AssetCard UnscrupulousLoan3
unscrupulousLoan3 = assetWith UnscrupulousLoan3 Cards.unscrupulousLoan3 cannotLeavePlay

instance HasAbilities UnscrupulousLoan3 where
  getAbilities (UnscrupulousLoan3 a) =
    controlled a 1 (youExist can.gain.resources) (freeReaction $ PlayCard #after You $ isThisCard a)
      : [ controlled a 2 (youExist $ InvestigatorWithResources $ lessThan 10) $ forced (eliminationWindow iid)
        | iid <- toList a.owner
        ]

instance RunMessage UnscrupulousLoan3 where
  runMessage msg a@(UnscrupulousLoan3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 10
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      exile attrs
      pure a
    _ -> UnscrupulousLoan3 <$> liftRunMessage msg attrs
