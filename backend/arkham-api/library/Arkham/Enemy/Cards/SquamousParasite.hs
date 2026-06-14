module Arkham.Enemy.Cards.SquamousParasite (squamousParasite) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Message.Lifted.Placement

newtype SquamousParasite = SquamousParasite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

squamousParasite :: EnemyCard SquamousParasite
squamousParasite = enemy SquamousParasite Cards.squamousParasite (3, Static 1, 3) (1, 0)

instance HasAbilities SquamousParasite where
  getAbilities (SquamousParasite a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)
      , mkAbility a 2 $ forced $ EnemyLeavesPlay #when (be a)
      ]

instance RunMessage SquamousParasite where
  runMessage msg e@(SquamousParasite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Flip this enemy and resolve its text (the glyph back)."
      -- The back side (story code 11580b) is currently registered only as the
      -- enemy's double-sided face, not as a readable Story card, so we resolve the
      -- known glyph effect directly here.
      flipOver iid attrs
      -- TODO: once 11580b is implemented as the proper story/back side, resolve its
      -- text here (likely via readStory) instead of the inlined glyph translation.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      -- TODO: the rune_t translated word is unverified; "Death" is a placeholder.
      campaignSpecific "translateGlyph" ("Squamous Parasite" :: Text, "Death" :: Text)
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- "If Squamous Parasite would leave play, set it aside, out of play."
      place attrs (OutOfPlay SetAsideZone)
      pure e
    _ -> SquamousParasite <$> liftRunMessage msg attrs
