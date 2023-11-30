module Arkham.Asset.Cards.FirstAid3 (firstAid3, FirstAid3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (allInvestigators)
import Arkham.Classes.HasGame
import Arkham.Damage
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude

newtype FirstAid3 = FirstAid3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid3 :: AssetCard FirstAid3
firstAid3 = assetWith FirstAid3 Cards.firstAid3 (whenNoUsesL ?~ DiscardWhenNoUses)

healableAsset :: Sourceable source => source -> DamageType -> LocationMatcher -> AssetMatcher
healableAsset (toSource -> source) hType loc = HealableAsset source hType $ at_ loc <> AssetControlledBy (affectsOthers Anyone)

instance HasAbilities FirstAid3 where
  getAbilities (FirstAid3 x) = [controlledAbility x 1 criteria $ actionAbilityWithCost (assetUseCost x Supply 1)]
   where
    healable hType = HealableInvestigator (toSource x) hType $ at_ YourLocation
    criteria =
      oneOf
        [ exists $ oneOf $ map healable [#horror, #damage]
        , exists $ oneOf $ map (\hType -> healableAsset x hType YourLocation) [#damage, #horror]
        ]

componentLabel :: Targetable target => GameTokenType -> target -> [Message] -> UI Message
componentLabel component (toTarget -> target) = case target of
  InvestigatorTarget iid' -> ComponentLabel (InvestigatorComponent iid' component)
  AssetTarget aid -> ComponentLabel (AssetComponent aid component)
  _ -> error "unhandled target"

damageComponentLabel :: Targetable target => target -> Source -> UI Message
damageComponentLabel (toTarget -> thing) source = componentLabel DamageToken thing [HealDamage thing source 1]

getInvestigatorChoices :: HasGame m => InvestigatorId -> PlayerId -> Source -> m [UI Message]
getInvestigatorChoices iid player source = do
  horrorInvestigators <- select $ HealableInvestigator source #horror $ colocatedWith iid
  damageInvestigators <- select $ HealableInvestigator source #damage $ colocatedWith iid
  for (setToList $ horrorInvestigators <> damageInvestigators) $ \i -> do
    let target = toTarget i
    mHealHorror <- runMaybeT do
      guard $ i `member` horrorInvestigators
      MaybeT $ getHealHorrorMessage source 1 i
    pure
      $ targetLabel i
      $ [ chooseOneAtATime player
            $ [damageComponentLabel target source | i `member` damageInvestigators]
            <> [componentLabel HorrorToken target [healHorror] | healHorror <- maybeToList mHealHorror]
        ]

getAssetChoices :: HasGame m => InvestigatorId -> PlayerId -> Source -> m [UI Message]
getAssetChoices iid player source = do
  horrorAssets <- select $ healableAsset source #horror (locationWithInvestigator iid)
  damageAssets <- select $ healableAsset source #damage (locationWithInvestigator iid)

  pure $ flip map (setToList $ horrorAssets <> damageAssets) \asset' -> do
    targetLabel
      asset'
      [ chooseOneAtATime player
          $ [damageComponentLabel asset' source | asset' `member` damageAssets]
          <> [ componentLabel HorrorToken asset' [HealHorror (toTarget asset') source 1]
             | asset' `member` horrorAssets
             ]
      ]

instance RunMessage FirstAid3 where
  runMessage msg (FirstAid3 attrs) =
    FirstAid3 <$> case msg of
      UseThisAbility iid (isSource attrs -> True) 1 -> do
        -- [ALERT] [SAME] MedicalStudent
        let source = attrs.ability 1
        player <- getPlayer iid
        investigatorChoices <- getInvestigatorChoices iid player source
        assetChoices <- getAssetChoices iid player source
        push $ chooseOne player $ assetChoices <> investigatorChoices
        pure attrs
      _ -> runMessage msg attrs
