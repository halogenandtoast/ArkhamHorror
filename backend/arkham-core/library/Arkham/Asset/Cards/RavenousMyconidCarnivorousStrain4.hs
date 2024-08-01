module Arkham.Asset.Cards.RavenousMyconidCarnivorousStrain4 (
  ravenousMyconidCarnivorousStrain4,
  RavenousMyconidCarnivorousStrain4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Matcher
import Arkham.Token

newtype RavenousMyconidCarnivorousStrain4 = RavenousMyconidCarnivorousStrain4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousMyconidCarnivorousStrain4 :: AssetCard RavenousMyconidCarnivorousStrain4
ravenousMyconidCarnivorousStrain4 =
  assetWith
    RavenousMyconidCarnivorousStrain4
    Cards.ravenousMyconidCarnivorousStrain4
    ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities RavenousMyconidCarnivorousStrain4 where
  getAbilities (RavenousMyconidCarnivorousStrain4 a) =
    [ playerLimit PerRound
        $ controlledAbility
          a
          1
          (youExist $ InvestigatorWithBondedCard $ cardIs Events.uncannyGrowth)
        $ FastAbility Free
    , restrictedAbility a 2 ControlsThis
        $ FastAbility
          ( ChooseEnemyCost
              $ NonEliteEnemy
              <> EnemyAt YourLocation
              <> EnemyWithRemainingHealth (atMost $ a.use Growth)
          )
    ]

instance RunMessage RavenousMyconidCarnivorousStrain4 where
  runMessage msg a@(RavenousMyconidCarnivorousStrain4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      uncannyGrowth <- searchBondedJust iid Events.uncannyGrowth
      addToHand iid [uncannyGrowth]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ (chosenEnemyPayment -> Just eid) -> do
      defeatEnemy eid iid (attrs.ability 1)
      removeTokens (attrs.ability 2) attrs Growth (attrs.use Growth)
      pure a
    _ -> RavenousMyconidCarnivorousStrain4 <$> liftRunMessage msg attrs
