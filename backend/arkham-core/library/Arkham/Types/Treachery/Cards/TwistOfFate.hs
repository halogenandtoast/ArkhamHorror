module Arkham.Types.Treachery.Cards.TwistOfFate
  ( TwistOfFate(..)
  , twistOfFate
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Token
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TwistOfFate = TwistOfFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistOfFate :: TreacheryCard TwistOfFate
twistOfFate = treachery TwistOfFate Cards.twistOfFate

instance (TreacheryRunner env) => RunMessage env TwistOfFate where
  runMessage msg t@(TwistOfFate attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RequestTokens source (Just iid) 1 SetAside)
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
      t <$ pushAll (msgs <> [ResetTokens source, Discard $ toTarget attrs])
    _ -> TwistOfFate <$> runMessage msg attrs
