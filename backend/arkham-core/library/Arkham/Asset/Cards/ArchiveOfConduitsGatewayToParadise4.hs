module Arkham.Asset.Cards.ArchiveOfConduitsGatewayToParadise4 (
  archiveOfConduitsGatewayToParadise4,
  ArchiveOfConduitsGatewayToParadise4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype ArchiveOfConduitsGatewayToParadise4 = ArchiveOfConduitsGatewayToParadise4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archiveOfConduitsGatewayToParadise4 :: AssetCard ArchiveOfConduitsGatewayToParadise4
archiveOfConduitsGatewayToParadise4 = asset ArchiveOfConduitsGatewayToParadise4 Cards.archiveOfConduitsGatewayToParadise4

instance HasAbilities ArchiveOfConduitsGatewayToParadise4 where
  getAbilities (ArchiveOfConduitsGatewayToParadise4 attrs) =
    [ controlledAbility attrs 1 (exists (be attrs <> AssetWithUses Leyline)) $ FastAbility Free
    , controlledAbility
        attrs
        2
        ( exists
            $ affectsOthers
            $ InvestigatorWithToken Token.Leyline
            <> oneOf
              ( can.draw.cards : [HealableInvestigator (attrs.ability 2) dType Anyone | dType <- [#horror, #damage]]
              )
        )
        actionAbility
    ]

instance RunMessage ArchiveOfConduitsGatewayToParadise4 where
  runMessage msg a@(ArchiveOfConduitsGatewayToParadise4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ affectsOthers Anyone
      chooseOrRunOne
        iid
        [ targetLabel investigator [MoveUses (toSource attrs) (toTarget investigator) Leyline 1]
        | investigator <- investigators
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      iids <-
        select
          $ affectsOthers
          $ InvestigatorWithToken Token.Leyline
          <> oneOf
            ( can.draw.cards : [HealableInvestigator (attrs.ability 2) dType Anyone | dType <- [#horror, #damage]]
            )
      chooseOrRunOne
        iid
        [targetLabel iid' [HandleTargetChoice iid (attrs.ability 2) (toTarget iid')] | iid' <- iids]
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) _ -> do
      chooseOne iid [Label "Use Leyline" [DoStep 2 msg], Label "Do not Leyline" [DoStep 1 msg]]
      pure a
    DoStep n (HandleTargetChoice _iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid')) -> do
      let source = attrs.ability 2
      drawCardsIfCan iid' (attrs.ability 2) n
      canHealHorror <- canHaveHorrorHealed source iid'
      canHealDamage <- canHaveDamageHealed source iid'
      when (canHealHorror || canHealDamage) $ do
        chooseOrRunOne iid'
          $ [DamageLabel iid' [HealDamage (toTarget iid') source n] | canHealDamage]
          <> [HorrorLabel iid' [HealHorror (toTarget iid') source n] | canHealHorror]
      pure a
    _ -> ArchiveOfConduitsGatewayToParadise4 <$> liftRunMessage msg attrs
