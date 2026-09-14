{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Registration helpers used by generated homebrew card-entry modules.
module Arkham.Homebrew.CardRegistry where

import Arkham.Act.Types (ActCard, IsAct, SomeActCard (..))
import Arkham.Agenda.Types (AgendaCard, IsAgenda, SomeAgendaCard (..))
import Arkham.Asset.Types (AssetCard, IsAsset, SomeAssetCard (..))
import Arkham.Enemy.Types (EnemyCard, IsEnemy, SomeEnemyCard (..))
import Arkham.Homebrew.Types
import Arkham.Location.Types (IsLocation, LocationCard, SomeLocationCard (..))
import Arkham.Prelude
import Arkham.Skill.Types (IsSkill, SkillCard, SomeSkillCard (..))
import Arkham.Story.Types (IsStory, SomeStoryCard (..), StoryCard)
import Arkham.Treachery.Types (IsTreachery, SomeTreacheryCard (..), TreacheryCard)

class IsHomebrewCard a where
  homebrewCard :: HomebrewContent

actContent :: IsAct a => ActCard a -> HomebrewContent
actContent card = mempty {acts = [SomeActCard card]}

agendaContent :: IsAgenda a => AgendaCard a -> HomebrewContent
agendaContent card = mempty {agendas = [SomeAgendaCard card]}

assetContent :: IsAsset a => AssetCard a -> HomebrewContent
assetContent card = mempty {assets = [SomeAssetCard card]}

enemyContent :: IsEnemy a => EnemyCard a -> HomebrewContent
enemyContent card = mempty {enemies = [SomeEnemyCard card]}

locationContent :: IsLocation a => LocationCard a -> HomebrewContent
locationContent card = mempty {locations = [SomeLocationCard card]}

skillContent :: IsSkill a => SkillCard a -> HomebrewContent
skillContent card = mempty {skills = [SomeSkillCard card]}

storyContent :: IsStory a => StoryCard a -> HomebrewContent
storyContent card = mempty {stories = [SomeStoryCard card]}

treacheryContent :: IsTreachery a => TreacheryCard a -> HomebrewContent
treacheryContent card = mempty {treacheries = [SomeTreacheryCard card]}
