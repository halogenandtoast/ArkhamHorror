module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.SongYin (songYin) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (instrumentInPlay)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Percussion)
import Arkham.Matcher

newtype SongYin = SongYin EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songYin :: EnemyCard SongYin
songYin = enemy SongYin Cards.songYin

instance HasModifiersFor SongYin where
  getModifiersFor (SongYin a) = do
    unlocked <- instrumentInPlay Percussion
    modifySelfWhen a (not unlocked) [CannotBeDamaged]

instance HasAbilities SongYin where
  getAbilities (SongYin a) =
    extend1 a
      $ restricted a 1 (OnSameLocation <> exists (TreacheryWithTrait Percussion <> InPlayTreachery))
      $ parleyAction_

instance RunMessage SongYin where
  runMessage msg (SongYin attrs) = runQueueT $ case msg of
    {- "Test [combat] (2) three times. If you succeed at all three skill tests:
    Flip this card over." The run of passes is counted in the enemy's own meta;
    any failure resets it, so the three have to be consecutive within one parley. -}
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      test iid
      pure . SongYin $ attrs {enemyMeta = toJSON (1 :: Int)}
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let passes = toResultDefault (0 :: Int) (enemyMeta attrs)
      if passes >= 3
        then do
          card <- fetchCard Stories.percussionistsMuse
          readStory iid card Stories.percussionistsMuse
          pure . SongYin $ attrs {enemyMeta = toJSON (0 :: Int)}
        else do
          test iid
          pure . SongYin $ attrs {enemyMeta = toJSON (passes + 1)}
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) ->
      pure . SongYin $ attrs {enemyMeta = toJSON (0 :: Int)}
    _ -> SongYin <$> liftRunMessage msg attrs
   where
    test iid = do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 2)
