module Arkham.Act.Cards.KingdomOfTheSkai (KingdomOfTheSkai (..), kingdomOfTheSkai) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Port))

newtype KingdomOfTheSkai = KingdomOfTheSkai ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- Search the encounter deck, discard pile, and all play areas for Cats of
-- Ulthar, each Pack of Vooniths, and each card from the Zoogs encounter set,
-- and remove them from the game.

-- Shuffle each set-aside copy of Priest of a Thousand Masks into the encounter
-- deck, along with the encounter discard pile.

kingdomOfTheSkai :: ActCard KingdomOfTheSkai
kingdomOfTheSkai =
  act
    (1, A)
    KingdomOfTheSkai
    Cards.kingdomOfTheSkai
    (Just $ GroupClueCost (PerPlayer 2) (LocationWithTrait Port))

instance RunMessage KingdomOfTheSkai where
  runMessage msg a@(KingdomOfTheSkai attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      players <- getAllPlayers
      push
        $ storyWithChooseOne
          lead
          players
          "You find a captain willing to grant you passage to the remote regions of the Dreamlands, wherein you may find signs from the gods to point you in the direction of Kadath."
          [ Label "Visit the isle of Oriab to the south. Resolve _Oriab Setup_ in the Campaign Guide." []
          , Label "Visit the ancient land of Mnar to the west. Resolve _Mnar Setup_ in the Campaign Guide." []
          , Label
              "Visit the Forbidden Lands to the north. Resolve _Forbidden Lands Setup_ in the Campaign Guide."
              []
          , Label
              "Visit the kingdom of the Timeless Realm to the east. Resolve _Timeless Realm Setup_ in the Campaign Guide."
              []
          ]
      pure a
    _ -> KingdomOfTheSkai <$> runMessage msg attrs
