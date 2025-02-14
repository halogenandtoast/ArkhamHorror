module Arkham.Event.Events.CorrelateAllItsContents (correlateAllItsContents) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher

newtype CorrelateAllItsContents = CorrelateAllItsContents EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

correlateAllItsContents :: EventCard CorrelateAllItsContents
correlateAllItsContents = event CorrelateAllItsContents Cards.correlateAllItsContents

instance HasAbilities CorrelateAllItsContents where
  getAbilities (CorrelateAllItsContents a) =
    [ restricted a 1 ControlsThis
        $ ConstantReaction
          "Cancel Token"
          (RevealChaosToken #after You FirstChaosTokenRevealedThisSkillTest)
          Free
    ]

instance RunMessage CorrelateAllItsContents where
  runMessage msg e@(CorrelateAllItsContents attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      -- This is constant so not really an ability
      cancelChaosToken attrs iid token
      returnChaosTokens [token]
      unfocusChaosTokens
      drawAnotherChaosToken iid
      pure e
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      investigate sid iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      let controlledBy = AssetControlledBy (affectsOthers $ colocatedWith attrs.owner)
      chargeAssets <- select $ controlledBy <> AssetWithUseType Charge
      secretAssets <- select $ controlledBy <> AssetWithUseType Secret
      let amount = if n == 1 || n == 3 then 2 else 1
      unless (null chargeAssets && null secretAssets) do
        chooseOneM iid do
          targets chargeAssets \aid -> addUses attrs aid Charge amount
          targets secretAssets \aid -> addUses attrs aid Secret amount
      pure e
    _ -> CorrelateAllItsContents <$> liftRunMessage msg attrs
