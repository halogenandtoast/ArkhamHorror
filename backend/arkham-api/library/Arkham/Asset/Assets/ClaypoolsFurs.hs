module Arkham.Asset.Assets.ClaypoolsFurs (claypoolsFurs) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Window
import Arkham.Matcher

newtype ClaypoolsFurs = ClaypoolsFurs AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

claypoolsFurs :: AssetCard ClaypoolsFurs
claypoolsFurs = assetWith ClaypoolsFurs Cards.claypoolsFurs (healthL ?~ 3)

instance HasAbilities ClaypoolsFurs where
  getAbilities (ClaypoolsFurs a) =
    [ restricted a 1 (ControlsThis <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ ReactionAbility (RevealChaosToken #when You #frost) (DamageCost (a.ability 1) (toTarget a) 1)
    ]

instance RunMessage ClaypoolsFurs where
  runMessage msg a@(ClaypoolsFurs attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> t) _ -> do
      cancelChaosToken (attrs.ability 1) t
      pushAll
        [ ReturnChaosTokens [t]
        , UnfocusChaosTokens
        , DrawAnotherChaosToken iid
        , RerunSkillTest
        ]
      pure a
    _ -> ClaypoolsFurs <$> liftRunMessage msg attrs
