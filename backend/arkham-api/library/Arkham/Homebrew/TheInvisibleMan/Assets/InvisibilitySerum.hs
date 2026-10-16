module Arkham.Homebrew.TheInvisibleMan.Assets.InvisibilitySerum (invisibilitySerum) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.I18n
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype InvisibilitySerum = InvisibilitySerum AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

invisibilitySerum :: AssetCard InvisibilitySerum
invisibilitySerum = asset InvisibilitySerum Cards.invisibilitySerum

instance HasAbilities InvisibilitySerum where
  getAbilities (InvisibilitySerum a) =
    [ scenarioI18n
        $ withI18nTooltip "invisibilitySerum.vanish"
        $ reaction a 1 ControlsThis (assetUseCost a Supply 1)
        $ oneOf [AttemptToFight #when You AnyEnemy, AttemptToEvade #when You AnyEnemy]
    ]

-- | The test and the enemy being attacked or evaded, from either window.
getDetails :: [Window] -> (SkillTestId, EnemyId)
getDetails [] = error "no attacked or evaded enemy"
getDetails ((windowType -> Window.AttemptToFightEnemy sid _ eid) : _) = (sid, eid)
getDetails ((windowType -> Window.AttemptToEvadeEnemy sid _ eid) : _) = (sid, eid)
getDetails (_ : rest) = getDetails rest

instance RunMessage InvisibilitySerum where
  runMessage msg a@(InvisibilitySerum attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDetails -> (sid, eid)) _ -> do
      chooseNM iid 2 $ scenarioI18n $ scope "invisibilitySerum" do
        labeled "weakenFight" do
          skillTestModifiers sid (attrs.ability 1) eid [EnemyFight (-2), RemoveKeyword Keyword.Retaliate]
        labeled "weakenEvade" do
          skillTestModifiers sid (attrs.ability 1) eid [EnemyEvade (-2), RemoveKeyword Keyword.Alert]
        labeled "damage" do
          onSucceedByEffect sid (atLeast 0) (attrs.ability 1) sid $ doStep 1 msg
        labeled "move" do
          onSucceedByEffect sid (atLeast 0) (attrs.ability 1) sid $ doStep 2 msg
      pure a
    -- Both successes resolve from their step rather than from the captured
    -- body, so the damage guard and the destinations are read after the test.
    DoStep 1 (UseCardAbility iid (isSource attrs -> True) 1 (getDetails -> (_, eid)) _) -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      pure a
    DoStep 2 (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      ls <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseOrRunOneM iid $ targets ls $ moveTo (attrs.ability 1) iid
      pure a
    _ -> InvisibilitySerum <$> liftRunMessage msg attrs
