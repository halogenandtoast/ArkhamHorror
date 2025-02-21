module Arkham.Asset.Assets.DialOfAncientsSignsOfAberration4 (dialOfAncientsSignsOfAberration4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Window
import Arkham.Matcher

newtype DialOfAncientsSignsOfAberration4 = DialOfAncientsSignsOfAberration4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dialOfAncientsSignsOfAberration4 :: AssetCard DialOfAncientsSignsOfAberration4
dialOfAncientsSignsOfAberration4 = asset DialOfAncientsSignsOfAberration4 Cards.dialOfAncientsSignsOfAberration4

-- Note: ideally we would treat this as one ability instead of two, but we need
-- triggers on whether or not we are doing a fight or evade so we must make it
-- two
instance HasAbilities DialOfAncientsSignsOfAberration4 where
  getAbilities (DialOfAncientsSignsOfAberration4 a) =
    [ restricted a 1 ControlsThis fightAction_
    , restricted a 2 ControlsThis evadeAction_
    , limitedAbility NoLimit
        $ restricted a 3 ControlsThis
        $ ConstantReaction
          "Spend 1 charge to cancel that token, return it to the bag, and reveal a new one"
          (RevealChaosToken #after Anyone $ CancelableChaosToken AnyChaosToken)
          (assetUseCost a Charge 1)
    ]

instance RunMessage DialOfAncientsSignsOfAberration4 where
  runMessage msg a@(DialOfAncientsSignsOfAberration4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseEvadeEnemy sid iid (attrs.ability 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 3 (getChaosToken -> token) _ -> do
      whenJustM getSkillTestInvestigator \iid' -> do
        cancelChaosToken (attrs.ability 1) iid token
        pushAll
          [ ReturnChaosTokens [token]
          , UnfocusChaosTokens
          , DrawAnotherChaosToken iid'
          , RerunSkillTest
          ]
      pure a
    _ -> DialOfAncientsSignsOfAberration4 <$> liftRunMessage msg attrs
