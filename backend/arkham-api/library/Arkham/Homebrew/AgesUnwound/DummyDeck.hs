{- | The campaign's true-solo dummy deck (campaign guide p2, "Changes for Solo
Play").

A few Ages Unwound cards have you interact with /another investigator's deck/.
In true solo there is no such deck, so the guide has you build one out of the
core of the four classes you are not playing and interact with that instead.

> Remove any cards of your class from this deck, as well as any cards you have
> at least one copy of in your deck.

The guide's "if you do not own any of the cards on this list, make appropriate
replacements from your collection" is a caveat about a physical collection, so
the construction here is entirely mechanical and needs no prompt.

== The convention for cards added later

A card that reads "another investigator's deck" must fall back to this deck when
it finds no other investigator rather than doing nothing:

> others <- select $ not_ (InvestigatorWithId iid) <> can.manipulate.deck
> if null others
>   then searchDummyDeck iid attrs 3 1
>   else chooseOrRunOneM iid $ targets others \other -> ...

and the scenario it appears in must call 'setUpDummyDeck' in its setup block.
'Arkham.Homebrew.AgesUnwound.Treacheries.SkillsOfAnotherLife' and
'Arkham.Homebrew.AgesUnwound.Agendas.ArmyOfOne' are the two existing call sites,
so Scenarios II and IV are the only ones that build the deck today. /Player/
cards that touch another investigator's deck are deliberately out of scope.

Nothing else is needed: the guide's "when a card from the dummy deck would leave
play, instead shuffle it back into the dummy deck" is handled once, in
'Arkham.Homebrew.AgesUnwound.Campaign', and so applies in every scenario.
-}
module Arkham.Homebrew.AgesUnwound.DummyDeck (
  dummyDeckDefs,
  isDummyDeckCard,
  searchDummyDeck,
  setUpDummyDeck,
) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern DummyDeck)
import Arkham.Id (InvestigatorId)
import Arkham.Investigator.Types (Field (InvestigatorClass, InvestigatorDeck))
import Arkham.Matcher
import Arkham.Message.Lifted (search)
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Name (toTitle)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Setup (ScenarioBuilderT, addExtraDeck)
import Arkham.Skill.Cards qualified as Skills
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Arkham.Zone

{- | The 25 cards the guide lists: per class, "2 assets, 2 events and 1 skill,
representing the core of that class". The guide's "Dark Prophesy" is Dark
Prophecy.
-}
dummyDeckDefs :: [CardDef]
dummyDeckDefs =
  [ -- Guardian
    Assets.fortyFiveAutomatic
  , Assets.beatCop
  , Events.emergencyAid
  , Events.onTheHunt
  , Skills.viciousBlow
  , -- Seeker
    Assets.hawkEyeFoldingCamera
  , Assets.oldBookOfLore
  , Events.barricade
  , Events.connectTheDots
  , Skills.deduction
  , -- Rogue
    Assets.burglary
  , Assets.lockpicks
  , Events.narrowEscape
  , Events.sneakAttack
  , Skills.quickThinking
  , -- Mystic
    Assets.riteOfSeeking
  , Assets.shrivelling
  , Events.darkProphecy
  , Events.drawnToTheFlame
  , Skills.fearless
  , -- Survivor
    Assets.gravediggersShovel
  , Assets.rabbitsFoot
  , Events.hidingSpot
  , Events.scroungeForSupplies
  , Skills.riseToTheOccasion
  ]

{- | Build the dummy deck. A no-op outside true solo, so it is safe to call from
any setup block.
-}
setUpDummyDeck :: ReverseQueue m => ScenarioBuilderT m ()
setUpDummyDeck =
  getInvestigators >>= \case
    [iid] -> do
      yourClass <- field InvestigatorClass iid
      yourCards <- fieldMap InvestigatorDeck (map (toTitle . toCardDef) . toList) iid
      let
        -- Compared by title so that running an upgraded copy still counts as
        -- having the card: Deduction (2) in your deck removes Deduction here.
        yours def = yourClass `member` cdClassSymbols def || toTitle def `elem` yourCards
      addExtraDeck DummyDeck =<< shuffleM (filter (not . yours) dummyDeckDefs)
    _ -> pure ()

{- | Whether a card came out of the dummy deck.

Two things make this answerable without remembering card ids: the deck is
generated rather than dealt from a decklist, so its cards have no owner while
everything in an investigator's deck is stamped with one (see
'Arkham.Decklist'); and the deck is built by excluding every card the
investigator already has, so no title is ever in both places. An investigator
count would be the less reliable test -- eliminating a player in a two-handed
game would make it answer "true solo".
-}
isDummyDeckCard :: Card -> Bool
isDummyDeckCard card = isNothing card.owner && toCardDef card `elem` dummyDeckDefs

{- | "Search the top @n@ cards of another investigator's deck for @k@
non-weakness, non-signature cards and draw them. Shuffle the searched deck." --
against the dummy deck, and silent when there is none.

'AddFoundToHand' rather than 'DrawFound': a search of a /scenario/ deck only
draws encounter cards ('Arkham.Scenario.Runner'), and for a player card found
there, being put into hand is the whole of what drawing it does. The one thing
it is not is a draw, so a card of yours that triggers on drawing will not see
these.
-}
searchDummyDeck
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> Int -> m ()
searchDummyDeck iid source n k = do
  dummy <- getScenarioDeck DummyDeck
  unless (null dummy) do
    search
      iid
      source
      (ScenarioDeckTarget DummyDeck)
      [(FromTopOfDeck n, ShuffleBackIn)]
      (basic $ NonWeakness <> not_ SignatureCard)
      (AddFoundToHand iid k)
