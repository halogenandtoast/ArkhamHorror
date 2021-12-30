module Arkham.Effect.Effects.StormOfSpirits
  ( StormOfSpirits(..)
  , stormOfSpirits
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Token

newtype StormOfSpirits = StormOfSpirits EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits :: EffectArgs -> StormOfSpirits
stormOfSpirits = StormOfSpirits . uncurry4 (baseAttrs "03153")

instance HasModifiersFor env StormOfSpirits

instance
  ( HasQueue env
  , HasId LocationId env InvestigatorId
  , Query InvestigatorMatcher env
  )
  => RunMessage env StormOfSpirits where
  runMessage msg e@(StormOfSpirits attrs) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget attrs -> do
      e <$ when
        (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        do
          lid <- getId iid
          iids <- selectList $ InvestigatorAt $ LocationWithId lid
          pushAll
            $ [ InvestigatorAssignDamage iid' (effectSource attrs) DamageAny 1 0
              | iid' <- iids
              ]
            <> [DisableEffect $ toId attrs]
    SkillTestEnds _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> StormOfSpirits <$> runMessage msg attrs
