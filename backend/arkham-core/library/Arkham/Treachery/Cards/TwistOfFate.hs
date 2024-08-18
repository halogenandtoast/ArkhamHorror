module Arkham.Treachery.Cards.TwistOfFate (TwistOfFate (..), twistOfFate) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TwistOfFate = TwistOfFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistOfFate :: TreacheryCard TwistOfFate
twistOfFate = treachery TwistOfFate Cards.twistOfFate

instance RunMessage TwistOfFate where
  runMessage msg t@(TwistOfFate attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      pure t
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      let
        source = toSource attrs
        msgs =
          mapMaybe
            ( \case
                ElderSign -> Nothing
                PlusOne -> Nothing
                Zero -> Just (InvestigatorAssignDamage iid source DamageAny 1 0)
                MinusOne ->
                  Just (InvestigatorAssignDamage iid source DamageAny 1 0)
                MinusTwo ->
                  Just (InvestigatorAssignDamage iid source DamageAny 1 0)
                MinusThree ->
                  Just (InvestigatorAssignDamage iid source DamageAny 1 0)
                MinusFour ->
                  Just (InvestigatorAssignDamage iid source DamageAny 1 0)
                MinusFive ->
                  Just (InvestigatorAssignDamage iid source DamageAny 1 0)
                MinusSix ->
                  Just (InvestigatorAssignDamage iid source DamageAny 1 0)
                MinusSeven ->
                  Just (InvestigatorAssignDamage iid source DamageAny 1 0)
                MinusEight ->
                  Just (InvestigatorAssignDamage iid source DamageAny 1 0)
                Skull -> Just (InvestigatorAssignDamage iid source DamageAny 0 2)
                Cultist ->
                  Just (InvestigatorAssignDamage iid source DamageAny 0 2)
                Tablet ->
                  Just (InvestigatorAssignDamage iid source DamageAny 0 2)
                ElderThing ->
                  Just (InvestigatorAssignDamage iid source DamageAny 0 2)
                AutoFail ->
                  Just (InvestigatorAssignDamage iid source DamageAny 0 2)
                CurseToken -> Nothing
                BlessToken -> Nothing
            )
            faces
      pushAll (msgs <> [ResetChaosTokens source])
      pure t
    _ -> TwistOfFate <$> runMessage msg attrs
