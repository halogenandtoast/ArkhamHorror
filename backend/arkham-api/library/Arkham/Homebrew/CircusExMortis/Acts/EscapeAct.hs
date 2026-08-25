module Arkham.Homebrew.CircusExMortis.Acts.EscapeAct (escapeActAdvance) where

import Arkham.Act.Import.Lifted
import Arkham.Card.CardDef
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Location.Grid (GridLocation (..))
import Arkham.Matcher

{- | Under Suspicion and Under Their Noses differ only in which Camp Outskirts
they bring into play.
-}
escapeActAdvance :: ReverseQueue m => ActAttrs -> CardDef -> m ()
escapeActAdvance attrs campOutskirts = do
  -- "Remove each copy of Kidnapped Citizen in play from the game (including
  -- ones flipped to their story card sides)": the card is double-sided across
  -- types, so sweep both entity kinds.
  for_ kidnappedCitizenDefs \def -> selectEach (storyIs def) removeFromGame
  selectEach (AssetWithTitle "Kidnapped Citizen") removeFromGame
  drawFuryTokenForDirection >>= traverse_ \direction -> do
    lid <- placeSetAsideLocation campOutskirts
    push $ PlaceGrid $ GridLocation (furyDirectionOutwardPos direction) lid
  advanceActDeck attrs

kidnappedCitizenDefs :: [CardDef]
kidnappedCitizenDefs =
  [ Stories.hiddenInPlainSight
  , Stories.underLockAndKey
  , Stories.cautiousJailers
  , Stories.deepInTheDark
  , Stories.clappedInIrons
  , Stories.hypnoticState
  ]
