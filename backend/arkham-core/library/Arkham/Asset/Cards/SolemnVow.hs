module Arkham.Asset.Cards.SolemnVow (
  solemnVow,
  SolemnVow (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Projection

newtype SolemnVow = SolemnVow AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

solemnVow :: AssetCard SolemnVow
solemnVow = asset SolemnVow Cards.solemnVow

instance HasAbilities SolemnVow where
  getAbilities (SolemnVow attrs) =
    [ restrictedAbility attrs 1 ControlsThis (FastAbility Free)
        `withCriteria` exists (You <> oneOf [InvestigatorHasCardWithDamage, InvestigatorHasCardWithHorror])
        `withCriteria` exists (affectsOthers $ InvestigatorAt YourLocation <> OwnsAsset (be attrs))
    ]

instance RunMessage SolemnVow where
  runMessage msg a@(SolemnVow attrs) = case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      owner <- field AssetOwner (toId attrs)
      player <- getPlayer iid
      when (Just iid == owner) $ do
        iids <- selectList $ colocatedWith iid <> NotInvestigator (be iid)
        push $ chooseOrRunOne player $ targetLabels iids $ only . (`TakeControlOfAsset` (toId attrs))
      SolemnVow <$> runMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasDamage <- iid <=~> InvestigatorHasCardWithDamage
      hasHorror <- iid <=~> InvestigatorHasCardWithHorror

      damageAssets <- selectList $ assetControlledBy iid <> AssetWithHealth
      horrorAssets <- selectList $ assetControlledBy iid <> AssetWithSanity

      iid' <- selectJust $ OwnsAsset (be attrs)
      damageAssets' <- selectList $ assetControlledBy iid' <> AssetWithHealth
      horrorAssets' <- selectList $ assetControlledBy iid' <> AssetWithSanity

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
