module Arkham.Asset.Assets.TrackShoes (trackShoes, TrackShoes (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype TrackShoes = TrackShoes AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trackShoes :: AssetCard TrackShoes
trackShoes = asset TrackShoes Cards.trackShoes

instance HasModifiersFor TrackShoes where
  getModifiersFor (TrackShoes a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities TrackShoes where
  getAbilities (TrackShoes a) =
    [ skillTestAbility
        $ restricted a 1 ControlsThis
        $ ReactionAbility (MovedButBeforeEnemyEngagement #after You Anywhere) (exhaust a)
    ]

instance RunMessage TrackShoes where
  runMessage msg a@(TrackShoes attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      accessible <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid accessible $ moveTo (attrs.ability 1) iid
      pure a
    _ -> TrackShoes <$> liftRunMessage msg attrs
