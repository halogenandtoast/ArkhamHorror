module Arkham.Asset.Assets.HeavyFurs (heavyFurs, HeavyFurs (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher

newtype HeavyFurs = HeavyFurs AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heavyFurs :: AssetCard HeavyFurs
heavyFurs = assetWith HeavyFurs Cards.heavyFurs (healthL ?~ 2)

instance HasAbilities HeavyFurs where
  getAbilities (HeavyFurs attrs) =
    [ controlledAbility attrs 1 (DuringSkillTest $ YourSkillTest #any)
        $ ReactionAbility
          (RevealChaosToken #after You (not_ #autofail))
          (DamageCost (attrs.ability 1) (toTarget attrs) 1)
    ]

instance RunMessage HeavyFurs where
  runMessage msg a@(HeavyFurs attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      cancelChaosToken (attrs.ability 1) token
      pushAll
        [ ReturnChaosTokens [token]
        , UnfocusChaosTokens
        , DrawAnotherChaosToken iid
        ]
      pure a
    _ -> HeavyFurs <$> liftRunMessage msg attrs
