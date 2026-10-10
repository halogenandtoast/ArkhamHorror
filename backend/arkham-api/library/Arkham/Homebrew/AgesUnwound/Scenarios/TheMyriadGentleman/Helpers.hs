{- | Scenario-local helpers for Scenario II, The Myriad Gentleman.

Everything here exists for the scenario's signature mechanic, "Copies of
Enemies" (guide p6):

> During this scenario, investigators may be instructed to spawn copies of a
> displayed enemy. To do this, that investigator places the top card of their
> deck into play in their threat area, treating it as a copy of the specified
> enemy until it leaves play (if it is ever unclear who should do this, the lead
> investigator does so). When that enemy leaves play, the card's owner places it
> on the bottom of their deck.

A copy is a real enemy entity built from the /displayed/ enemy's def
('Enemies.theMyriadGentleman_042') carrying the player card's 'CardId' — the
same trick the swarm machinery uses at @Enemy/Runner.hs:388@. It deliberately
does /not/ use 'Arkham.Placement.AsSwarm': swarm placement drags
host-redirected movement, engagement and exhaustion, and "the host cannot be
defeated while swarm cards remain", none of which these cards print. The copy
sits at an ordinary placement instead ('InThreatArea' when engaged, the location
otherwise).

The @(enemy, owner, card)@ triple lives in the scenario's meta, because nothing
on the enemy remembers which player card it was made from once the def has been
swapped in. The scenario owns the two halves that the triple exists for:

* @RemoveEnemy@ — put the card on the bottom of its owner's deck.
* @Discarded (EnemyTarget …)@ — suppress the default bookkeeping, which would
  otherwise drop a Myriad Gentleman /encounter/ card into the encounter discard
  pile (see 'Arkham.Scenario.Runner', the single-sided branch).
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers where

import Arkham.Card
import Arkham.Card.EncounterCard (lookupEncounterCard)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (selectAny)
import Arkham.Enemy.Creation (IsEnemyCreationMethod)
import Arkham.Helpers.Message (createEnemyWith)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.Helpers qualified as AgesUnwound
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Matcher
import Arkham.Message (Message (ObtainCard))
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Message.Lifted.Scenario (scenarioSpecific)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioMeta))
import Arkham.ScenarioLogKey (ScenarioLogKey (HomebrewScenarioLogKey))
import GHC.Records

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = AgesUnwound.scenarioI18n "theMyriadGentleman" a

-- | One in-play copy of The Myriad Gentleman and the deck card standing in for it.
data MyriadCopy = MyriadCopy
  { myriadCopyEnemy :: EnemyId
  , myriadCopyOwner :: InvestigatorId
  , myriadCopyCard :: Card
  , myriadCopyReturned :: Bool
  {- ^ Set once the card has gone back to the bottom of its owner's deck, so a
  second @RemoveEnemy@ for the same id cannot return it twice. The entry
  itself is kept, because the @Discarded@ suppression above has to keep
  recognising the id after the enemy is gone.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "enemy" MyriadCopy EnemyId where
  getField = myriadCopyEnemy

instance HasField "owner" MyriadCopy InvestigatorId where
  getField = myriadCopyOwner

instance HasField "card" MyriadCopy Card where
  getField = myriadCopyCard

instance HasField "returned" MyriadCopy Bool where
  getField = myriadCopyReturned

newtype MyriadMeta = MyriadMeta {myriadCopies :: [MyriadCopy]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "copies" MyriadMeta [MyriadCopy] where
  getField = myriadCopies

emptyMyriadMeta :: MyriadMeta
emptyMyriadMeta = MyriadMeta []

getMyriadMeta :: HasGame m => m MyriadMeta
getMyriadMeta = toResultDefault emptyMyriadMeta <$> scenarioField ScenarioMeta

-- | The message key the scenario answers to add one copy to its meta.
registerMyriadCopyKey :: Text
registerMyriadCopyKey = "theMyriadGentleman.registerCopy"

-- | ... and the one that marks a copy's card as already sent home.
returnedMyriadCopyKey :: Text
returnedMyriadCopyKey = "theMyriadGentleman.returnedCopy"

{- | Agenda 1b and the scenario reference's @[tablet]@ both gate on "it is act 2
or 3".
-}
onActTwoOrThree :: HasGame m => m Bool
onActTwoOrThree = anyM (selectAny . ActWithStep) [2, 3]

{- | Agenda 1b: "Remember that \"he is ready for you.\"" Act 1b reads it to give
the lead investigator an extra wave of copies.
-}
heIsReadyForYou :: ScenarioLogKey
heIsReadyForYou = HomebrewScenarioLogKey "agesUnwound.HeIsReadyForYou"

{- | Spawn @n@ copies of The Myriad Gentleman, each made from the top card of
@iid@'s deck. @method@ is where they spawn: pass an 'InvestigatorId' for
"engaged with them" and a 'LocationId' for "at that location".

Fewer than @n@ copies appear when the deck runs out — a card you do not have is
a card you cannot place.
-}
spawnMyriadCopies
  :: (ReverseQueue m, IsEnemyCreationMethod method)
  => InvestigatorId -> Int -> method -> m ()
spawnMyriadCopies iid n method = do
  cards <- take n <$> fieldMap InvestigatorDeck (.cards) iid
  for_ cards \pc -> do
    let
      copyCard =
        EncounterCard
          $ (lookupEncounterCard Enemies.theMyriadGentleman_042 pc.id) {ecOwner = Just iid}
    (eid, create) <- createEnemyWith copyCard method id
    scenarioSpecific registerMyriadCopyKey (MyriadCopy eid iid (PlayerCard pc) False)
    push $ ObtainCard pc.id
    push create

-- | "spawn a copy of The Myriad Gentleman engaged with you".
spawnMyriadCopiesEngagedWith :: ReverseQueue m => InvestigatorId -> Int -> m ()
spawnMyriadCopiesEngagedWith iid n = spawnMyriadCopies iid n iid

{- | "spawn N copies of The Myriad Gentleman at <location>", with @iid@ supplying
the cards (the lead investigator wherever the card does not say who).
-}
spawnMyriadCopiesAt :: ReverseQueue m => InvestigatorId -> Int -> LocationId -> m ()
spawnMyriadCopiesAt iid n lid = spawnMyriadCopies iid n lid
