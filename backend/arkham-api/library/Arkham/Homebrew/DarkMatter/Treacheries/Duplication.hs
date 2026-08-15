module Arkham.Homebrew.DarkMatter.Treacheries.Duplication (duplication) where

import Arkham.Card
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Virtual)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (AdditionalRevelations))
import Arkham.Treachery.Import.Lifted

newtype Duplication = Duplication TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duplication :: TreacheryCard Duplication
duplication = treachery Duplication Cards.duplication

{- | Deferred hand-off: the drawn enemy has to be in play before it can host a
swarm card.
-}
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
an additional time. If it is an enemy, place this card under it as a swarm card."
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
        -- AdditionalRevelations rides the card into ResolveTreachery, which
        -- repeats the revelation itself but keeps a single After (Revelation ...)
        -- around it, so the treachery is still discarded (and resolved) once.
        if toCardType card == TreacheryType
          then temporaryModifier card attrs (AdditionalRevelations 1) $ drawCard iid card
          else do
            drawCard iid card
            when (toCardType card == EnemyType)
              $ push
              $ CampaignSpecific swarmKey (toJSON (attrs.id, toCardId card))
      pure t
    -- the drawn enemy has spawned by now, so slide this card under it. A swarm
    -- card is a copy of its host enemy, so this has to leave play as a treachery
    -- and come back as a swarm enemy; @RemoveTreachery@ also pops the pending
    -- @After Revelation@ that would otherwise discard the card instead.
    CampaignSpecific k (maybeResult -> Just (tid, cid))
      | k == swarmKey
      , tid == attrs.id -> do
          selectOne (EnemyWithCardId cid) >>= traverse_ \eid -> do
            push $ RemoveTreachery attrs.id
            push $ PlacedSwarmCard eid (toCard attrs)
          pure t
    _ -> Duplication <$> liftRunMessage msg attrs
