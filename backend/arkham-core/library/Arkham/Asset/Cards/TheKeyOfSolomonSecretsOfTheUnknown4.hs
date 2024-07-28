module Arkham.Asset.Cards.TheKeyOfSolomonSecretsOfTheUnknown4 (
  theKeyOfSolomonSecretsOfTheUnknown4,
  TheKeyOfSolomonSecretsOfTheUnknown4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Asset
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype TheKeyOfSolomonSecretsOfTheUnknown4 = TheKeyOfSolomonSecretsOfTheUnknown4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKeyOfSolomonSecretsOfTheUnknown4 :: AssetCard TheKeyOfSolomonSecretsOfTheUnknown4
theKeyOfSolomonSecretsOfTheUnknown4 = asset TheKeyOfSolomonSecretsOfTheUnknown4 Cards.theKeyOfSolomonSecretsOfTheUnknown4

instance HasAbilities TheKeyOfSolomonSecretsOfTheUnknown4 where
  getAbilities (TheKeyOfSolomonSecretsOfTheUnknown4 a) =
    [ controlledAbility
        a
        1
        ( HasMoreBlessThanCurseTokens
            <> oneOf
              [ any_
                  [ HealableInvestigator (a.ability 1) kind $ InvestigatorAt YourLocation
                  | kind <- [#damage, #horror]
                  ]
              , any_
                  [ HealableAsset (a.ability 1) kind $ AssetAt YourLocation <> #ally
                  | kind <- [#damage, #horror]
                  ]
              ]
        )
        $ FastAbility (exhaust a <> ReturnChaosTokensToPoolCost 1 #bless)
    , controlledAbility a 2 (HasMoreCurseThanBlessTokens <> can.gain.resources You)
        $ FastAbility (exhaust a <> ReturnChaosTokensToPoolCost 1 #curse)
    ]

instance RunMessage TheKeyOfSolomonSecretsOfTheUnknown4 where
  runMessage msg a@(TheKeyOfSolomonSecretsOfTheUnknown4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <-
        selectTargets
          $ oneOf
            [ HealableAsset (attrs.ability 1) kind $ assetAtLocationWith iid <> #ally
            | kind <- [#damage, #horror]
            ]
      investigators <-
        selectTargets
          $ oneOf
            [ HealableInvestigator (attrs.ability 1) kind $ InvestigatorAt YourLocation
            | kind <- [#damage, #horror]
            ]
      chooseOneToHandle iid (attrs.ability 1) (assets <> investigators)
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      doStep 2 msg
      pure a
    DoStep n msg'@(HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (AssetTarget aid)) | n > 0 -> do
      canHealHorror <- assetCanHaveHorrorHealed (attrs.ability 1) aid
      canHealDamage <- assetCanHaveDamageHealed (attrs.ability 1) aid

      chooseOne
        iid
        $ [ Label "Heal damage" [HealDamage (AssetTarget aid) (attrs.ability 1) 1, DoStep (n - 1) msg']
          | canHealDamage
          ]
        <> [ Label "Heal horror" [HealHorror (AssetTarget aid) (attrs.ability 1) 1, DoStep (n - 1) msg']
           | canHealHorror
           ]
        <> [Label "Done Healing" []]
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (InvestigatorTarget aid) -> do
      doStep 2 msg
      pure a
    DoStep n msg'@(HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid')) | n > 0 -> do
      canHealHorror <- canHaveHorrorHealed (attrs.ability 1) iid'
      canHealDamage <- canHaveDamageHealed (attrs.ability 1) iid'

      chooseOne
        iid
        $ [ Label "Heal damage" [HealDamage (InvestigatorTarget iid') (attrs.ability 1) 1, DoStep (n - 1) msg']
          | canHealDamage
          ]
        <> [ Label "Heal horror" [HealHorror (InvestigatorTarget iid') (attrs.ability 1) 1, DoStep (n - 1) msg']
           | canHealHorror
           ]
        <> [Label "Done Healing" []]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainResourcesIfCan iid (attrs.ability 2) 2
      pure a
    _ -> TheKeyOfSolomonSecretsOfTheUnknown4 <$> liftRunMessage msg attrs
