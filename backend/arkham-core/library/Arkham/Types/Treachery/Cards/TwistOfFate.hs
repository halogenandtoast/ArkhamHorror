module Arkham.Types.Treachery.Cards.TwistOfFate
  ( TwistOfFate(..)
  , twistOfFate
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Token
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId

newtype TwistOfFate = TwistOfFate TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistOfFate :: TreacheryId -> a -> TwistOfFate
twistOfFate uuid _ = TwistOfFate $ baseAttrs uuid "02093"

instance HasModifiersFor env TwistOfFate where
  getModifiersFor = noModifiersFor

instance HasActions env TwistOfFate where
  getActions i window (TwistOfFate attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env TwistOfFate where
  runMessage msg t@(TwistOfFate attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (RequestTokens source (Just iid) 1 SetAside)
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
            Cultist -> Just (InvestigatorAssignDamage iid source DamageAny 0 2)
            Tablet -> Just (InvestigatorAssignDamage iid source DamageAny 0 2)
            ElderThing ->
              Just (InvestigatorAssignDamage iid source DamageAny 0 2)
            AutoFail ->
              Just (InvestigatorAssignDamage iid source DamageAny 0 2)
          )
          tokens
      t <$ unshiftMessages
        (msgs <> [ResetTokens source, Discard $ toTarget attrs])
    _ -> TwistOfFate <$> runMessage msg attrs
