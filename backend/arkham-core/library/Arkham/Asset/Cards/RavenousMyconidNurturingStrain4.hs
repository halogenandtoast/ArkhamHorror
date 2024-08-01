{-# LANGUAGE MultiWayIf #-}

module Arkham.Asset.Cards.RavenousMyconidNurturingStrain4 (
  ravenousMyconidNurturingStrain4,
  RavenousMyconidNurturingStrain4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Token

newtype RavenousMyconidNurturingStrain4 = RavenousMyconidNurturingStrain4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousMyconidNurturingStrain4 :: AssetCard RavenousMyconidNurturingStrain4
ravenousMyconidNurturingStrain4 =
  assetWith
    RavenousMyconidNurturingStrain4
    Cards.ravenousMyconidNurturingStrain4
    ((healthL ?~ 3) . (sanityL ?~ 3))

instance HasAbilities RavenousMyconidNurturingStrain4 where
  getAbilities (RavenousMyconidNurturingStrain4 a) =
    [ withTooltip "Search your bonded cards for Uncanny Growth and add it to your hand."
        $ playerLimit PerRound
        $ controlledAbility
          a
          1
          (youExist $ InvestigatorWithBondedCard $ cardIs Events.uncannyGrowth)
        $ FastAbility Free
    , withTooltip
        "Either heal that much damage/horror from Ravenous Myconid, or move that much damage/horror from investigators or _Ally_ assets at your location to Ravenous Myconid"
        $ controlledAbility
          a
          2
          ( exists (be a <> AssetWithUseCount Growth (atLeast 1))
              <> oneOf
                [ exists (oneOf [HealableAsset (a.ability 2) kind (be a) | kind <- [#damage, #horror]])
                , exists
                    ( affectsOthers
                        $ InvestigatorAt YourLocation
                        <> oneOf [InvestigatorWithAnyDamage, InvestigatorWithAnyHorror]
                    )
                , exists (AssetAt YourLocation <> #ally <> oneOf [AssetWithDamage, AssetWithHorror])
                ]
          )
        $ FastAbility (assetUseCost a Growth $ a.use Growth)
    ]

instance RunMessage RavenousMyconidNurturingStrain4 where
  runMessage msg a@(RavenousMyconidNurturingStrain4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      uncannyGrowth <- searchBondedJust iid Events.uncannyGrowth
      addToHand iid [uncannyGrowth]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ (totalUsesPayment -> n) -> do
      hasDamage <- attrs.id <=~> oneOf [AssetWithDamage, AssetWithHorror]
      canMoveDamage <-
        liftA2
          (||)
          ( selectAny
              $ affectsOthers
              $ colocatedWith iid
              <> oneOf [InvestigatorWithAnyDamage, InvestigatorWithAnyHorror]
          )
          ( selectAny
              $ AssetAt (locationWithInvestigator iid)
              <> #ally
              <> oneOf [AssetWithDamage, AssetWithHorror]
          )
      chooseOrRunOne iid
        $ [ Label "Heal that much damage/horror from Ravenous Myconid" [DoStep n (ForChoice 1 msg)] | hasDamage
          ]
        <> [ Label
            "Move that much damage/horror from investigator or Ally assets at your location to Ravenous Myconid"
            [DoStep n (ForChoice 2 msg), CheckDefeated (attrs.ability 2) (toTarget attrs)]
           | canMoveDamage
           ]
      pure a
    DoStep n msg'@(ForChoice 1 (UseThisAbility iid (isSource attrs -> True) 2)) | n > 0 -> do
      damage <- field AssetDamage attrs.id
      horror <- field AssetHorror attrs.id
      if
        | damage + horror <= n -> do
            pushWhen (damage > 0) $ HealDamage (toTarget attrs) (attrs.ability 2) damage
            pushWhen (horror > 0) $ HealHorror (toTarget attrs) (attrs.ability 2) horror
        | damage == 0 && horror > 0 -> push $ HealHorror (toTarget attrs) (attrs.ability 2) (min n horror)
        | horror == 0 && damage > 0 -> push $ HealDamage (toTarget attrs) (attrs.ability 2) (min n damage)
        | damage == 0 && horror == 0 -> pure ()
        | otherwise -> do
            chooseOne
              iid
              [ AssetDamageLabel attrs.id [HealDamage (toTarget attrs) (attrs.ability 2) 1]
              , AssetHorrorLabel attrs.id [HealHorror (toTarget attrs) (attrs.ability 2) 1]
              ]

            doStep (n - 1) msg'
      pure a
    DoStep n msg'@(ForChoice 2 (UseThisAbility iid (isSource attrs -> True) 2)) | n > 0 -> do
      maxDamage <- fromMaybe 0 <$> field AssetRemainingHealth attrs.id
      maxHorror <- fromMaybe 0 <$> field AssetRemainingSanity attrs.id
      di <-
        if maxDamage == 0
          then pure []
          else
            selectWithField InvestigatorDamage (affectsOthers $ colocatedWith iid <> InvestigatorWithAnyDamage)
      hi <-
        if maxHorror == 0
          then pure []
          else
            selectWithField InvestigatorHorror (affectsOthers $ colocatedWith iid <> InvestigatorWithAnyHorror)
      da <-
        if maxDamage == 0
          then pure []
          else
            selectWithField
              AssetDamage
              (AssetAt (locationWithInvestigator iid) <> #ally <> AssetWithDamage)
      ha <-
        if maxHorror == 0
          then pure []
          else
            selectWithField
              AssetHorror
              (AssetAt (locationWithInvestigator iid) <> #ally <> AssetWithHorror)

      unless (null di && null hi && null da && null ha) do
        let totalDamage = sum $ map snd di <> map snd da
        let totalHorror = sum $ map snd hi <> map snd ha

        if totalDamage <= maxDamage && totalHorror <= maxHorror && totalDamage + totalHorror <= n
          then do
            for_ di \(iid', dmg) -> moveTokens (attrs.ability 2) iid' attrs Damage dmg
            for_ hi \(iid', hrr) -> moveTokens (attrs.ability 2) iid' attrs Horror hrr
            for_ da \(aid', dmg) -> moveTokens (attrs.ability 2) aid' attrs Damage dmg
            for_ ha \(aid', hrr) -> moveTokens (attrs.ability 2) aid' attrs Horror hrr
          else do
            case (di, hi, da, ha) of
              ([(iid', _)], [], [], []) -> moveTokens (attrs.ability 2) iid' attrs Damage (min maxDamage n)
              ([], [(iid', _)], [], []) -> moveTokens (attrs.ability 2) iid' attrs Horror (min maxHorror n)
              ([], [], [(aid', _)], []) -> moveTokens (attrs.ability 2) aid' attrs Damage (min maxDamage n)
              ([], [], [], [(aid', _)]) -> moveTokens (attrs.ability 2) aid' attrs Horror (min maxHorror n)
              _ -> chooseOneM iid do
                for_ di \(iid', _) -> do
                  damageLabeled iid do
                    moveTokensNoDefeated (attrs.ability 2) iid' attrs Damage 1
                    doStep (n - 1) msg'

                for_ hi \(iid', _) -> do
                  horrorLabeled iid do
                    moveTokensNoDefeated (attrs.ability 2) iid' attrs Horror 1
                    doStep (n - 1) msg'

                for_ da \(aid', _) -> do
                  assetDamageLabeled aid' do
                    moveTokensNoDefeated (attrs.ability 2) aid' attrs Damage 1
                    doStep (n - 1) msg'

                for_ ha \(aid', _) -> do
                  assetHorrorLabeled aid' do
                    moveTokensNoDefeated (attrs.ability 2) aid' attrs Horror 1
                    doStep (n - 1) msg'

      pure a
    _ -> RavenousMyconidNurturingStrain4 <$> liftRunMessage msg attrs
