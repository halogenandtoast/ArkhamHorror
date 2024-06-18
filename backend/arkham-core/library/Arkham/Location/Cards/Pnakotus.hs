module Arkham.Location.Cards.Pnakotus (pnakotus, Pnakotus (..)) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Discover
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection

newtype Pnakotus = Pnakotus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pnakotus :: LocationCard Pnakotus
pnakotus = location Pnakotus Cards.pnakotus 2 (Static 3)

instance HasAbilities Pnakotus where
  getAbilities (Pnakotus a) =
    withBaseAbilities
      a
      [ groupLimit PerGame
          $ restrictedAbility
            a
            1
            (Here <> ChaosTokenCountIs (IncludeSealed $ ChaosTokenFaceIs Tablet) (atLeast 3))
            actionAbility
      ]

instance RunMessage Pnakotus where
  runMessage msg l@(Pnakotus attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      clues <- field LocationClues (toId l)
      let drawing = newCardDraw attrs iid clues
      pushAll [Msg.DiscoverClues iid $ discover attrs (attrs.ability 1) clues, DrawCards iid drawing]
      pure l
    _ -> Pnakotus <$> runMessage msg attrs
