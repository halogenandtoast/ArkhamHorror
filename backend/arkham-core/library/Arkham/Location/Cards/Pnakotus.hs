module Arkham.Location.Cards.Pnakotus (
  pnakotus,
  Pnakotus (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Token

newtype Pnakotus = Pnakotus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pnakotus :: LocationCard Pnakotus
pnakotus = location Pnakotus Cards.pnakotus 2 (Static 3)

instance HasAbilities Pnakotus where
  getAbilities (Pnakotus a) =
    withBaseAbilities
      a
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility
            a
            1
            ( Here
                <> TokenCountIs
                  (IncludeSealed $ TokenFaceIs Tablet)
                  (AtLeast $ Static 3)
            )
          $ ActionAbility Nothing
          $ ActionCost 1
      ]

instance RunMessage Pnakotus where
  runMessage msg l@(Pnakotus attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      clues <- field LocationClues (toId l)
      drawing <- newCardDraw iid (toSource attrs) clues
      pushAll [Msg.DiscoverClues iid (toId l) (toAbilitySource attrs 1) clues Nothing, DrawCards drawing]
      pure l
    _ -> Pnakotus <$> runMessage msg attrs
