module Arkham.Campaigns.TheDrownedCity.Effects.StruggleForAir where

import Arkham.Classes.Entity (Entity)
import Arkham.Classes.HasAbilities (HasAbilities)
import Arkham.Classes.HasModifiersFor (HasModifiersFor)
import Arkham.Classes.Query
import Arkham.Classes.RunMessage.Internal (RunMessage (..), liftRunMessage)
import Arkham.Effect.Import
import Arkham.Effect.Types qualified as Effect
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message (Message (..))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Target

newtype StruggleForAirEffect = StruggleForAirEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

struggleForAirEffect :: EffectArgs -> StruggleForAirEffect
struggleForAirEffect (effectId, builder) =
  StruggleForAirEffect $ Effect.baseAttrs "struggleForAir" effectId builder

instance RunMessage StruggleForAirEffect where
  runMessage msg e@(StruggleForAirEffect attrs) = runQueueT $ case msg of
    EndTurn iid | isTarget iid attrs.target -> do
      chooseOneM iid
        $ withI18n
        $ scope "theDrownedCity"
        $ labeledI18n "noAir"
        $ directDamage iid attrs.source 5
      disableReturn e
    DisableEffect _ -> StruggleForAirEffect <$> liftRunMessage msg attrs
    _ -> do
      e' <- StruggleForAirEffect <$> liftRunMessage msg attrs
      case attrs.target.investigator of
        Nothing -> pure e'
        Just iid -> do
          -- The investigator finds air if they entered an unflooded or partially
          -- flooded location, or decreased the flood level of their current
          -- location. Decreasing a fully flooded location always makes it at
          -- most partially flooded, so both cases reduce to: the investigator is
          -- no longer at a fully flooded location.
          foundAir <- selectAny $ locationWithInvestigator iid <> not_ FullyFloodedLocation
          if foundAir then disableReturn e' else pure e'
