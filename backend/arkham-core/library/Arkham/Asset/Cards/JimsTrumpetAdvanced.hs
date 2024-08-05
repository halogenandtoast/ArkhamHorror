module Arkham.Asset.Cards.JimsTrumpetAdvanced (JimsTrumpetAdvanced (..), jimsTrumpetAdvanced) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype JimsTrumpetAdvanced = JimsTrumpetAdvanced AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jimsTrumpetAdvanced :: AssetCard JimsTrumpetAdvanced
jimsTrumpetAdvanced = asset JimsTrumpetAdvanced Cards.jimsTrumpetAdvanced

instance HasAbilities JimsTrumpetAdvanced where
  getAbilities (JimsTrumpetAdvanced x) =
    [ controlledAbility
        x
        1
        ( oneOf
            [ exists (HealableInvestigator (toSource x) #horror $ at_ (oneOf [YourLocation, ConnectedLocation]))
            , exists (HealableAsset (toSource x) #horror $ at_ (oneOf [YourLocation, ConnectedLocation]))
            ]
            <> DuringSkillTest (YourSkillTest AnySkillTest)
        )
        $ ReactionAbility (RevealChaosToken #when Anyone $ oneOf [#skull, #curse]) (exhaust x)
    ]

instance RunMessage JimsTrumpetAdvanced where
  runMessage msg a@(JimsTrumpetAdvanced attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOneToHandle iid (attrs.ability 1)
        $ oneOf [locationWithInvestigator iid, ConnectedFrom (locationWithInvestigator iid)]
        <> oneOf
          [ LocationWithInvestigator (HealableInvestigator (attrs.ability 1) #horror Anyone)
          , LocationWithAsset (HealableAsset (attrs.ability 1) #horror AnyAsset)
          ]
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror $ investigatorAt lid
      for_ investigators \iid -> healHorror iid (attrs.ability 1) 1

      assets <- select $ HealableAsset (attrs.ability 1) #horror $ assetAt lid
      for_ assets \aid -> healHorror aid (attrs.ability 1) 1

      pure a
    _ -> JimsTrumpetAdvanced <$> liftRunMessage msg attrs
