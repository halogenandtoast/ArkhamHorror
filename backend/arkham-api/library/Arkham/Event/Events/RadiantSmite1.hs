module Arkham.Event.Events.RadiantSmite1 (radiantSmite1, RadiantSmite1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype RadiantSmite1 = RadiantSmite1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

radiantSmite1 :: EventCard RadiantSmite1
radiantSmite1 = event RadiantSmite1 Cards.radiantSmite1

instance HasModifiersFor RadiantSmite1 where
  getModifiersFor (InvestigatorTarget iid) (RadiantSmite1 attrs) | attrs.owner == iid = do
    let n = count ((== #bless) . (.face)) (eventSealedChaosTokens attrs)
    toModifiers attrs $ guard (n > 0) *> [DamageDealt n, AnySkillValue n]
  getModifiersFor _ _ = pure []

instance RunMessage RadiantSmite1 where
  runMessage msg e@(RadiantSmite1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      n <- min 3 <$> selectCount (ChaosTokenFaceIs #bless)
      when (n > 0) do
        chooseAmounts
          iid
          "Number of Bless tokens to seal"
          (MaxAmountTarget n)
          [("Bless Tokens", (0, n))]
          attrs

      sid <- getRandom
      fight <- mkChooseFight sid iid attrs
      chooseOneM iid do
        labeled "Use {willpower}" $ push $ withSkillType #willpower fight
        labeled "Use {combat}" $ push fight
      pure e
    ResolveAmounts _iid (getChoiceAmount "Bless Tokens" -> n) (isTarget attrs -> True) -> do
      blessedTokens <- take n <$> select (ChaosTokenFaceIs #bless)
      for_ blessedTokens $ \blessedToken -> do
        pushAll [SealChaosToken blessedToken, SealedChaosToken blessedToken (toTarget attrs)]
      pure e
    Msg.EnemyDefeated _ _ (isSource attrs -> True) _ -> do
      pure $ RadiantSmite1 $ attrs & sealedChaosTokensL %~ filter ((/= #bless) . (.face))
    _ -> RadiantSmite1 <$> liftRunMessage msg attrs
