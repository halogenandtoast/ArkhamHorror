module Arkham.Homebrew.DarkMatter.Treacheries.Duplication (duplication) where

import Arkham.Card
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Virtual)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype Duplication = Duplication TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duplication :: TreacheryCard Duplication
duplication = treachery Duplication Cards.duplication

-- | Deferred hand-off so the swarm placement happens after the enemy has spawned.
swarmKey :: Text
swarmKey = "duplicationSwarm"

isVirtual :: Card -> Bool
isVirtual = member Virtual . cdCardTraits . toCardDef

{- | How deep to dig: everything down to and including the second [[Virtual]]
card. If the deck holds fewer than two, the whole deck is discarded.
-}
depthToSecondVirtual :: [Card] -> Int
depthToSecondVirtual = go (0 :: Int) 0
 where
  go found n = \case
    [] -> n
    (c : rest)
      | isVirtual c -> if found + 1 >= 2 then n + 1 else go (found + 1) (n + 1) rest
      | otherwise -> go found (n + 1) rest

{- | "Revelation - Discard cards from the top of the encounter deck until 2
[[Virtual]] encounter cards are discarded. Choose and draw one of the discarded
[[Virtual]] encounter cards. If it is a treachery, resolve its revelation effect
an additional time."
-}
instance RunMessage Duplication where
  runMessage msg t@(Duplication attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      Deck deck' <- getEncounterDeck
      let deck = map toCard deck'
      let n = depthToSecondVirtual deck
      when (n > 0) $ discardTopOfEncounterDeckAndHandle iid attrs n attrs
      pure t
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let virtuals = filter isVirtual (map toCard cards)
      focusCards virtuals $ chooseTargetM iid virtuals \card -> do
        unfocusCards
        drawCard iid card
        when (toCardType card == EnemyType)
          $ push
          $ CampaignSpecific swarmKey (toJSON (attrs.id, toCardId card))
      pure t
    -- the drawn enemy has spawned by now, so slide this card under it
    CampaignSpecific k (maybeResult -> Just (tid, cid))
      | k == swarmKey
      , tid == attrs.id -> do
          selectOne (EnemyWithCardId cid)
            >>= traverse_ \eid -> push $ PlaceTreachery attrs.id (AsSwarm eid $ toCard attrs)
          pure t
    _ -> Duplication <$> liftRunMessage msg attrs
