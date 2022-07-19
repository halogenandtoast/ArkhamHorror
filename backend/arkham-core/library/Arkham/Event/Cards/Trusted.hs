module Arkham.Event.Cards.Trusted
  ( trusted
  , Trusted(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype Trusted = Trusted EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trusted :: EventCard Trusted
trusted = event Trusted Cards.trusted

instance HasModifiersFor Trusted where
  getModifiersFor _ (AssetTarget aid) (Trusted a) =
    if AssetTarget aid `elem` eventAttachedTarget a
      then pure $ toModifiers a [HealthModifier 1, SanityModifier 1]
      else pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage Trusted where
  runMessage msg e@(Trusted attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <- selectListMap AssetTarget $ assetControlledBy iid <> AllyAsset
      push $ chooseOne
        iid
        [ TargetLabel target [AttachEvent eid target] | target <- targets ]
      pure e
    _ -> Trusted <$> runMessage msg attrs
