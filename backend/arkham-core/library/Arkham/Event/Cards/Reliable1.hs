module Arkham.Event.Cards.Reliable1
  ( reliable1
  , Reliable1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types ( Field (..) )
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait

newtype Reliable1 = Reliable1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reliable1 :: EventCard Reliable1
reliable1 = event Reliable1 Cards.reliable1

instance HasModifiersFor Reliable1 where
  getModifiersFor (InvestigatorTarget iid) (Reliable1 a) =
    case eventAttachedTarget a of
      Just (AssetTarget aid) -> do
        owner <- field AssetController aid
        if owner == Just iid
          then do
            abilities <- getActiveAbilities
            let isAttachedTargetAbility = (== AssetSource aid) . abilitySource
            pure $ toModifiers
              a
              [ AnySkillValue 1
              | any
                (and . sequence [isAttachedTargetAbility, isTriggeredAbility])
                abilities
              ]
          else pure []
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Reliable1 where
  runMessage msg e@(Reliable1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <- selectList $ assetControlledBy iid <> AssetWithTrait Item
      push $ chooseOne
        iid
        [ targetLabel asset [PlaceEvent iid eid $ AttachedToAsset asset Nothing]
        | asset <- assets
        ]
      pure e
    _ -> Reliable1 <$> runMessage msg attrs
