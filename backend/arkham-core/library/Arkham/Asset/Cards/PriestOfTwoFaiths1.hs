module Arkham.Asset.Cards.PriestOfTwoFaiths1 (priestOfTwoFaiths1, PriestOfTwoFaiths1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Prelude

newtype PriestOfTwoFaiths1 = PriestOfTwoFaiths1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

priestOfTwoFaiths1 :: AssetCard PriestOfTwoFaiths1
priestOfTwoFaiths1 = ally PriestOfTwoFaiths1 Cards.priestOfTwoFaiths1 (2, 2)

instance HasAbilities PriestOfTwoFaiths1 where
  getAbilities (PriestOfTwoFaiths1 x) =
    [ controlledAbility x 1 HasRemainingBlessTokens $ freeReaction $ AssetEntersPlay #when (be x)
    , restrictedAbility x 1 ControlsThis $ forced $ PhaseEnds #when #upkeep
    ]

instance RunMessage PriestOfTwoFaiths1 where
  runMessage msg a@(PriestOfTwoFaiths1 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- min 3 <$> getRemainingBlessTokens
      pushAll $ replicate n (AddChaosToken #bless)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      player <- getPlayer iid
      n <- getRemainingCurseTokens
      push
        $ chooseOrRunOne
          player
        $ [Label "Add 1 {curse} token to the chaos bag" [AddChaosToken #curse] | n > 0]
        <> [ Label "Discard Priest of Two Faiths" [toDiscardBy iid (attrs.ability 2) attrs]
           ]
      pure a
    _ -> PriestOfTwoFaiths1 <$> runMessage msg attrs
