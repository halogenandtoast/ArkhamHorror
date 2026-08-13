module Arkham.Homebrew.DarkMatter.Assets.David (david) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Direction
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Matcher
import Arkham.SkillType

{- | One of the four children hiding in Public School 187. Each is the front of a
double-sided card whose back is a "Reintegrated" story:

"[action] If The Boogeyman is at the location above or below David's location:
Parley. Test [SkillCombat] (2). If you succeed, flip him over and resolve the text
on the other side."
-}
newtype David = David AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

david :: AssetCard David
david = asset David Cards.david

-- | The Boogeyman is directly above or below this asset's location.
boogeymanVertical :: AssetAttrs -> Criterion
boogeymanVertical a =
  exists
    $ mapOneOf (\d -> LocationInDirection d (locationWithAsset a.id)) [Above, Below]
    <> LocationWithEnemy (enemyIs Enemies.theBOOGEYMAN)

instance HasAbilities David where
  getAbilities (David a) =
    [skillTestAbility $ restricted a 1 (boogeymanVertical a) parleyAction_]

instance RunMessage David where
  runMessage msg a@(David attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid SkillCombat (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- "Flip him over and resolve the text on the other side."
      readStory iid attrs Stories.reintegrated_063
      pure a
    _ -> David <$> liftRunMessage msg attrs
