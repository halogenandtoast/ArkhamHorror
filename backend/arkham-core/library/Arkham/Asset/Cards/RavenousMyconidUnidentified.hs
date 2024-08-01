module Arkham.Asset.Cards.RavenousMyconidUnidentified (
  ravenousMyconidUnidentified,
  RavenousMyconidUnidentified (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Matcher
import Arkham.Token

newtype RavenousMyconidUnidentified = RavenousMyconidUnidentified AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousMyconidUnidentified :: AssetCard RavenousMyconidUnidentified
ravenousMyconidUnidentified =
  assetWith
    RavenousMyconidUnidentified
    Cards.ravenousMyconidUnidentified
    ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities RavenousMyconidUnidentified where
  getAbilities (RavenousMyconidUnidentified a) =
    [ withTooltip "Search your bonded cards for Uncanny Growth and add it to you hand."
        $ controlledAbility
          a
          1
          (youExist $ InvestigatorWithBondedCard $ cardIs Events.uncannyGrowth)
          actionAbility
    , withTooltip
        "Move each growth to your resource pool, as resources. Record in your Campaign Log that \"you have classified a new species\""
        $ restrictedAbility a 2 (exists $ be a <> AssetWithUseCount Growth (atLeast 3)) actionAbility
    ]

instance RunMessage RavenousMyconidUnidentified where
  runMessage msg a@(RavenousMyconidUnidentified attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      uncannyGrowth <- searchBondedJust iid Events.uncannyGrowth
      addToHand iid [uncannyGrowth]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      moveTokens (attrs.ability 2) attrs (ResourceTarget iid) Growth (attrs.use Growth)
      record YouHaveClassifiedANewSpecies
      pure a
    _ -> RavenousMyconidUnidentified <$> liftRunMessage msg attrs
