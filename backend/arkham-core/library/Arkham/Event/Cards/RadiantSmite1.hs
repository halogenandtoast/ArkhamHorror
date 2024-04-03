module Arkham.Event.Cards.RadiantSmite1 (radiantSmite1, RadiantSmite1 (..)) where

import Arkham.Aspect
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Fight
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype RadiantSmite1 = RadiantSmite1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

radiantSmite1 :: EventCard RadiantSmite1
radiantSmite1 = event RadiantSmite1 Cards.radiantSmite1

instance HasModifiersFor RadiantSmite1 where
  getModifiersFor (InvestigatorTarget iid) (RadiantSmite1 attrs) | attrs.owner == iid = do
    let n = count ((== #bless) . (.face)) (eventSealedChaosTokens attrs)
    pure $ toModifiers attrs $ guard (n > 0) *> [DamageDealt n, AnySkillValue n]
  getModifiersFor _ _ = pure []

instance RunMessage RadiantSmite1 where
  runMessage msg e@(RadiantSmite1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      blessedTokens <- min 3 <$> selectCount (ChaosTokenFaceIs #bless)
      chooseFight <-
        leftOr <$> aspect iid attrs (#willpower `InsteadOf` #combat) (mkChooseFight iid attrs)
      player <- getPlayer iid
      pushAll
        $ [ chooseAmounts
            player
            "Number of Bless tokens to seal"
            (MaxAmountTarget blessedTokens)
            [("Bless Tokens", (0, blessedTokens))]
            attrs
          | blessedTokens > 0
          ]
        <> chooseFight
      pure e
    ResolveAmounts _iid (getChoiceAmount "Bless Tokens" -> n) (isTarget attrs -> True) -> do
      blessedTokens <- take n <$> select (ChaosTokenFaceIs #bless)
      for_ blessedTokens $ \blessedToken -> do
        pushAll [SealChaosToken blessedToken, SealedChaosToken blessedToken (toCard attrs)]

      pure e
    Msg.EnemyDefeated _ _ (isSource attrs -> True) _ -> do
      pure $ RadiantSmite1 $ attrs & sealedChaosTokensL %~ filter ((/= #bless) . (.face))
    _ -> RadiantSmite1 <$> runMessage msg attrs
