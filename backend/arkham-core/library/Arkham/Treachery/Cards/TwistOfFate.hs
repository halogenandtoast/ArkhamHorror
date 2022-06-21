module Arkham.Treachery.Cards.TwistOfFate
  ( TwistOfFate(..)
  , twistOfFate
  ) where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Message
import Arkham.RequestedTokenStrategy
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TwistOfFate = TwistOfFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistOfFate :: TreacheryCard TwistOfFate
twistOfFate = treachery TwistOfFate Cards.twistOfFate

instance RunMessage TwistOfFate where
  runMessage msg t@(TwistOfFate attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RequestTokens source (Just iid) (Reveal 1) SetAside)
    RequestedTokens source (Just iid) tokens | isSource attrs source -> do
      let
        msgs = mapMaybe
          (\case
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
          . tokenFace
          )
          tokens
      t <$ pushAll (msgs <> [ResetTokens source])
    _ -> TwistOfFate <$> runMessage msg attrs
