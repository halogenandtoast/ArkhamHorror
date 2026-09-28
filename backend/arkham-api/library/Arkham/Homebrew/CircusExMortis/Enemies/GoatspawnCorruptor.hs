module Arkham.Homebrew.CircusExMortis.Enemies.GoatspawnCorruptor (goatspawnCorruptor) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelfWhen)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers (hasSealedMoonToken)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype GoatspawnCorruptor = GoatspawnCorruptor EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goatspawnCorruptor :: EnemyCard GoatspawnCorruptor
goatspawnCorruptor = enemy GoatspawnCorruptor Cards.goatspawnCorruptor

savageAltar :: LocationMatcher
savageAltar = locationIs Locations.savageAltar

instance HasModifiersFor GoatspawnCorruptor where
  getModifiersFor (GoatspawnCorruptor a) = do
    modifySelectWhen a a.ready savageAltar [ShroudModifier 3]
    -- 'hasSealedMoonToken' reads the sealed tokens field, not modifiers
    moonlit <- selectAny $ investigatorEngagedWith a <> hasSealedMoonToken
    modifySelfWhen a moonlit [AddKeyword Keyword.Retaliate, AddKeyword Keyword.Alert]

instance HasAbilities GoatspawnCorruptor where
  getAbilities (GoatspawnCorruptor a) =
    extend1 a
      $ restricted a 1 (canDiscoverCluesAt savageAltar)
      $ freeReaction
      $ EnemyDefeated #after You ByAny (be a)

instance RunMessage GoatspawnCorruptor where
  runMessage msg e@(GoatspawnCorruptor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      discoverAtMatchingLocation_ iid (attrs.ability 1) savageAltar n
      pure e
    _ -> GoatspawnCorruptor <$> liftRunMessage msg attrs
