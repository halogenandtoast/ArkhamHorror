module Arkham.Asset.Cards.SolemnVow (
  solemnVow,
  SolemnVow (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Projection

newtype SolemnVow = SolemnVow AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

solemnVow :: AssetCard SolemnVow
solemnVow = asset SolemnVow Cards.solemnVow

instance HasAbilities SolemnVow where
  getAbilities (SolemnVow attrs) =
    [ controlledAbility
        attrs
        1
        ( exists (You <> AnyInvestigator [InvestigatorHasCardWithDamage, InvestigatorHasCardWithHorror])
            <> exists (InvestigatorAt YourLocation <> OwnsAsset (AssetWithId $ toId attrs))
        )
        $ FastAbility Free
    ]

instance RunMessage SolemnVow where
  runMessage msg a@(SolemnVow attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == toId attrs -> do
      owner <- field AssetOwner aid
      player <- getPlayer iid
      when (Just iid == owner) $ do
        iids <- selectList $ colocatedWith iid <> NotInvestigator (InvestigatorWithId iid)
        push $ chooseOrRunOne player $ targetLabels iids $ only . (`TakeControlOfAsset` aid)
      SolemnVow <$> runMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasDamage <- iid <=~> InvestigatorHasCardWithDamage
      hasHorror <- iid <=~> InvestigatorHasCardWithHorror

      damageAssets <- selectList $ AssetControlledBy (InvestigatorWithId iid) <> AssetWithHealth
      horrorAssets <- selectList $ AssetControlledBy (InvestigatorWithId iid) <> AssetWithSanity

      iid' <- selectJust $ OwnsAsset (AssetWithId $ toId attrs)
      damageAssets' <- selectList $ AssetControlledBy (InvestigatorWithId iid') <> AssetWithHealth
      horrorAssets' <- selectList $ AssetControlledBy (InvestigatorWithId iid') <> AssetWithSanity

      let
        damageChoices source =
          targetLabel iid' [MovedDamage source (toTarget iid') 1]
            : [ targetLabel aid' [MovedDamage source (toTarget aid') 1]
              | aid' <- damageAssets'
              ]
        horrorChoices source =
          targetLabel iid' [MovedHorror source (toTarget iid') 1]
            : [ targetLabel aid' [MovedHorror source (toTarget aid') 1]
              | aid' <- horrorAssets'
              ]

      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [DamageLabel iid [chooseOrRunOne player $ damageChoices (toSource iid)] | hasDamage]
        <> [HorrorLabel iid [chooseOrRunOne player $ horrorChoices (toSource iid)] | hasHorror]
        <> [AssetDamageLabel aid [chooseOrRunOne player $ damageChoices (toSource aid)] | aid <- damageAssets]
        <> [AssetHorrorLabel aid [chooseOrRunOne player $ horrorChoices (toSource aid)] | aid <- horrorAssets]

      pure a
    _ -> SolemnVow <$> runMessage msg attrs
