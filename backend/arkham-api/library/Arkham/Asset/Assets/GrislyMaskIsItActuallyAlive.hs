module Arkham.Asset.Assets.GrislyMaskIsItActuallyAlive (grislyMask) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrislyMaskIsItActuallyAlive = GrislyMaskIsItActuallyAlive AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyMask :: AssetCard GrislyMaskIsItActuallyAlive
grislyMask = asset GrislyMaskIsItActuallyAlive Cards.grislyMask

instance HasModifiersFor GrislyMaskIsItActuallyAlive where
  getModifiersFor (GrislyMaskIsItActuallyAlive a) = artifactModifiers a

-- Both printed abilities are bare [fast] abilities, so the buttons are only
-- distinguishable by their tooltips (issue #6034).
instance HasAbilities GrislyMaskIsItActuallyAlive where
  getAbilities (GrislyMaskIsItActuallyAlive x) =
    [ cardI18n (withI18nTooltip "grislyMask.disengage")
        $ controlled x 1 (whenOption "onlyWhenEngaged" $ exists (EnemyIsEngagedWith You))
        $ FastAbility (exhaust x)
    , -- Only usable once the glyphs naming this ability have been translated.
      cardI18n (withI18nTooltip "grislyMask.horrorForDamage")
        $ controlled
          x
          2
          ( glyphsAllKnown "JMSCB"
              <> exists (at_ YourLocation <> EnemyCanBeDamagedBySource (x.ability 2))
          )
        $ FastAbility (DirectHorrorCost (x.ability 2) You 1 <> exhaust x)
    , artifactAbility x 3
    ]

instance RunMessage GrislyMaskIsItActuallyAlive where
  runMessage msg a@(GrislyMaskIsItActuallyAlive attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      -- "you cannot move and cannot damage enemies or engage them". CannotBeEngaged is
      -- the engage clause: Game.CanEngageEnemy short-circuits on it. It also keeps the
      -- printed disengage from being undone on the spot, since a ready unengaged enemy
      -- re-engages any time it shares a location with an investigator.
      nextTurnModifiers iid (attrs.ability 1) iid [CannotMove, CannotDealDamage, CannotBeEngaged]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (attrs.ability 2)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 2) 2
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      handOffArtifact iid attrs
      pure a
    _ -> GrislyMaskIsItActuallyAlive <$> liftRunMessage msg attrs
