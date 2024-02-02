module Arkham.Event.Cards.CustomAmmunition3 (
  customAmmunition3,
  CustomAmmunition3 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait

newtype CustomAmmunition3 = CustomAmmunition3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

customAmmunition3 :: EventCard CustomAmmunition3
customAmmunition3 = event CustomAmmunition3 Cards.customAmmunition3

instance HasModifiersFor CustomAmmunition3 where
  getModifiersFor (InvestigatorTarget iid) (CustomAmmunition3 a) = do
    mSource <- getSkillTestSource
    mTarget <- getSkillTestTarget
    mAction <- getSkillTestAction
    case (mAction, mSource, mTarget) of
      (Just Action.Fight, Just (AssetSource aid), Just (EnemyTarget eid))
        | Just (AssetTarget aid) == eventAttachedTarget a ->
            do
              isMonster <- eid <=~> EnemyWithTrait Monster
              isController <- iid <=~> HasMatchingAsset (AssetWithId aid)
              pure $ toModifiers a [DamageDealt 1 | isMonster && isController]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage CustomAmmunition3 where
  runMessage msg e@(CustomAmmunition3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <-
        selectList
          $ AssetControlledBy (affectsOthers $ colocatedWith iid)
          <> AssetWithTrait Firearm
          <> NotAsset
            ( AssetWithAttachedEvent
                $ EventCardMatch
                $ cardIs
                  Cards.customAmmunition3
            )
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel
            asset
            [ PlaceEvent iid eid (AttachedToAsset asset Nothing)
            , AddUses asset Ammo 2
            ]
          | asset <- assets
          ]
      pure e
    _ -> CustomAmmunition3 <$> runMessage msg attrs
