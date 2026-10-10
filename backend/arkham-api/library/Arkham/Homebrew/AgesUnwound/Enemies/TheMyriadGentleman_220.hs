module Arkham.Homebrew.AgesUnwound.Enemies.TheMyriadGentleman_220 (theMyriadGentleman_220) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.Traits (pattern Myriad)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheMyriadGentleman_220 = TheMyriadGentleman_220 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMyriadGentleman_220 :: EnemyCard TheMyriadGentleman_220
theMyriadGentleman_220 = enemy TheMyriadGentleman_220 Cards.theMyriadGentleman_220

{- | "The Myriad Gentleman gets +1 fight for each [[Myriad]] swarm card at his
location (to a maximum of +3)."

A swarm card's location is its host's, so @EnemyAt@ already reaches the swarm
cards of every Myriad host standing here.
-}
instance HasModifiersFor TheMyriadGentleman_220 where
  getModifiersFor (TheMyriadGentleman_220 a) = do
    n <- selectCount $ IsSwarm <> EnemyWithTrait Myriad <> EnemyAt (locationWithEnemy a.id)
    modifySelfWhen a (n > 0) [EnemyFight (min 3 n)]

{- | "Forced - After the host Myriad Gentleman is defeated: Discard 1 swarm card
from each other [[Myriad]] host card in play."
-}
instance HasAbilities TheMyriadGentleman_220 where
  getAbilities (TheMyriadGentleman_220 a) =
    -- host-ness goes in the window, not in an ability criterion: at #after the
    -- enemy is out of play, and only the window's DefeatedEnemy branch re-reads
    -- the placement it was defeated with
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #after Anyone ByAny (be a <> IsHost)

instance RunMessage TheMyriadGentleman_220 where
  runMessage msg e@(TheMyriadGentleman_220 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      -- collapse to hosts first: swarm cards redirect their own messages to the
      -- host, so iterating them is not idempotent
      hosts <- select $ EnemyWithTrait Myriad <> IsHost <> not_ (be attrs)
      for_ hosts \host -> do
        swarm <- select $ SwarmOf host
        unless (null swarm) do
          chooseOneM lead $ targets swarm $ toDiscard (attrs.ability 1)
      pure e
    _ -> TheMyriadGentleman_220 <$> liftRunMessage msg attrs
