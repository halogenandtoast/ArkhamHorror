module Arkham.Homebrew.AgesUnwound.Enemies.BrainwashedExpedition (brainwashedExpedition) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Matcher

newtype BrainwashedExpedition = BrainwashedExpedition EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Massive and Swarming 2 are on the def; /Colour Out of Space/ puts it at Tunguska.
brainwashedExpedition :: EnemyCard BrainwashedExpedition
brainwashedExpedition = enemy BrainwashedExpedition Cards.brainwashedExpedition

{- | "__Forced__ - At the end of the round, if there are 4 or fewer swarm cards
under this card: Add 1 swarm card to Brainwashed Expedition. /
__Forced__ - When the host Brainwashed Expedition is defeated: Flip it over and
resolve its text."

Both abilities name the /host/, so both matchers exclude swarm cards -- a swarm
card is an enemy in its own right and would otherwise offer the Forced for every
card in the swarm.
-}
instance HasAbilities BrainwashedExpedition where
  getAbilities (BrainwashedExpedition a) =
    extend
      a
      [ restricted
          a
          1
          ( thisExists a (NotEnemy IsSwarm)
              <> EnemyCount (LessThanOrEqualTo $ Static 4) (SwarmOf a.id)
          )
          $ forced
          $ RoundEnds #when
      , mkAbility a 2 $ forced $ EnemyWouldBeDefeated #when (be a <> NotEnemy IsSwarm)
      ]

instance RunMessage BrainwashedExpedition where
  runMessage msg e@(BrainwashedExpedition attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      lead <- getLead
      push $ PlaceSwarmCards lead attrs.id 1
      pure e
    {- Printed as "when the host ... is defeated", hooked on
    @EnemyWouldBeDefeated@: /Window of Opportunity/ can "restore it to
    1[per_investigator] health", and @EnemyDefeated@ is pushed as a sibling of
    the pending @Defeated@ rather than wrapping it, so the discard can only be
    headed off from the /would/ window. The story's two surviving branches heal
    it back to full; the victory branch takes it out of play itself. -}
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      matchingDon't \case
        Defeated (EnemyTarget eid) _ _ _ -> eid == attrs.id
        _ -> False
      readStory iid attrs Stories.windowOfOpportunity
      pure e
    _ -> BrainwashedExpedition <$> liftRunMessage msg attrs
