module Arkham.Asset.Cards.TrackShoes (trackShoes, TrackShoes (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude

newtype TrackShoes = TrackShoes AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trackShoes :: AssetCard TrackShoes
trackShoes = asset TrackShoes Cards.trackShoes

instance HasModifiersFor TrackShoes where
  getModifiersFor (InvestigatorTarget iid) (TrackShoes attrs) | attrs `controlledBy` iid = do
    pure $ toModifiers attrs [SkillModifier #agility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities TrackShoes where
  getAbilities (TrackShoes attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility (MovedButBeforeEnemyEngagement #after You Anywhere) (exhaust attrs)
    ]

instance RunMessage TrackShoes where
  runMessage msg a@(TrackShoes attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      accessibleLocationIds <- getAccessibleLocations iid (attrs.ability 1)
      player <- getPlayer iid
      push $ chooseOne player [targetLabel lid [Move $ move attrs iid lid] | lid <- accessibleLocationIds]
      pure a
    _ -> TrackShoes <$> runMessage msg attrs
