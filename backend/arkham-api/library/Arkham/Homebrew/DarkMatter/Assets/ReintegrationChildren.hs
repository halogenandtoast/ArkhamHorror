module Arkham.Homebrew.DarkMatter.Assets.ReintegrationChildren (
  alma,
  david,
  tilde,
  william,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Card.CardCode (toCardCode)
import Arkham.Card.CardDef (CardDef)
import Arkham.Direction
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Matcher
import Arkham.SkillType

{- | The four children hiding in Public School 187. Each is the front of a
double-sided card whose back is a "Reintegrated" story:

"[action] If The Boogeyman is at the location above or below <name>'s location:
Parley. Test <skill> (2). If you succeed, flip her over and resolve the text on
the other side."

They differ only in which skill the parley tests.
-}
newtype ReintegrationChild = ReintegrationChild AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The Boogeyman is directly above or below this asset's location.
boogeymanVertical :: AssetAttrs -> Criterion
boogeymanVertical a =
  exists
    $ mapOneOf
      (\d -> LocationInDirection d (locationWithAsset a.id))
      [Above, Below]
    <> LocationWithEnemy (enemyIs Enemies.theBOOGEYMAN)

mkChild :: CardDef -> AssetCard ReintegrationChild
mkChild = asset ReintegrationChild

alma :: AssetCard ReintegrationChild
alma = mkChild Cards.alma

david :: AssetCard ReintegrationChild
david = mkChild Cards.david

tilde :: AssetCard ReintegrationChild
tilde = mkChild Cards.tilde

william :: AssetCard ReintegrationChild
william = mkChild Cards.william

-- | Alma tests willpower, David combat, Tilde intellect, William agility.
parleySkill :: AssetAttrs -> SkillType
parleySkill a
  | a.cardCode == toCardCode Cards.david = SkillCombat
  | a.cardCode == toCardCode Cards.tilde = SkillIntellect
  | a.cardCode == toCardCode Cards.william = SkillAgility
  | otherwise = SkillWillpower

-- | The "Reintegrated" story printed on this child's back.
childStory :: AssetAttrs -> CardDef
childStory a
  | a.cardCode == toCardCode Cards.david = Stories.reintegrated_063
  | a.cardCode == toCardCode Cards.tilde = Stories.reintegrated_064
  | a.cardCode == toCardCode Cards.william = Stories.reintegrated_065
  | otherwise = Stories.reintegrated_062

instance HasAbilities ReintegrationChild where
  getAbilities (ReintegrationChild a) =
    [skillTestAbility $ restricted a 1 (boogeymanVertical a) parleyAction_]

instance RunMessage ReintegrationChild where
  runMessage msg a@(ReintegrationChild attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid (parleySkill attrs) (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- "Flip her over and resolve the text on the other side."
      readStory iid attrs (childStory attrs)
      pure a
    _ -> ReintegrationChild <$> liftRunMessage msg attrs
