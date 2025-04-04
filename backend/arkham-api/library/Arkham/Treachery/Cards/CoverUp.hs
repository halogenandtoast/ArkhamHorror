module Arkham.Treachery.Cards.CoverUp (coverUp) where

import Arkham.Ability hiding (you)
import Arkham.Matcher
import Arkham.Script
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (DiscoverClues, sufferMentalTrauma)

newtype CoverUp = CoverUp TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coverUp :: TreacheryCard CoverUp
coverUp = treacheryWith CoverUp Cards.coverUp (tokensL %~ setTokens Clue 3)

instance HasAbilities CoverUp where
  getAbilities (CoverUp a) = mapFold a.owner \iid ->
    [ restricted a 1 (OnSameLocation <> CluesOnThis (atLeast 1))
        $ freeReaction (WouldDiscoverClues #when You YourLocation $ atLeast 1)
    , restricted a 2 (CluesOnThis $ atLeast 1) $ forcedOnElimination iid
    ]

instance RunMessage CoverUp where
  runMessage = script do
    revelation placeInYourThreatArea
    onAbility 1 $ insteadOfDiscoveringClues $ discardTokens #clue discoveredClues
    onAbility 2 $ sufferMentalTrauma 1
