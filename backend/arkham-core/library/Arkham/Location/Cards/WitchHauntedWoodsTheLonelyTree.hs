module Arkham.Location.Cards.WitchHauntedWoodsTheLonelyTree
  ( witchHauntedWoodsTheLonelyTree
  , WitchHauntedWoodsTheLonelyTree(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.Discard
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Projection

newtype WitchHauntedWoodsTheLonelyTree = WitchHauntedWoodsTheLonelyTree LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsTheLonelyTree :: LocationCard WitchHauntedWoodsTheLonelyTree
witchHauntedWoodsTheLonelyTree = location
  WitchHauntedWoodsTheLonelyTree
  Cards.witchHauntedWoodsTheLonelyTree
  2
  (PerPlayer 1)

instance HasModifiersFor WitchHauntedWoodsTheLonelyTree where
  getModifiersFor (InvestigatorTarget iid) (WitchHauntedWoodsTheLonelyTree a) =
    do
      handLength <- fieldMap InvestigatorHand length iid
      pure $ toModifiers
        a
        [ CannotInvestigateLocation (toId a)
        | handLength >= 3 && handLength <= 5
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities WitchHauntedWoodsTheLonelyTree where
  getAbilities (WitchHauntedWoodsTheLonelyTree a) = withBaseAbilities
    a
    [ limitedAbility (PlayerLimit PerRound 1)
      $ restrictedAbility a 1 Here
      $ FastAbility Free
    ]

instance RunMessage WitchHauntedWoodsTheLonelyTree where
  runMessage msg l@(WitchHauntedWoodsTheLonelyTree attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      handLength <- fieldMap InvestigatorHand length iid
      push $ chooseOne
        iid
        [ Label
          "You choose and discard 1 card from your hand, then an investigator at a different Witch-Haunted Woods draws 1 card"
          [toMessage $ chooseAndDiscardCard iid (toSource attrs)]
        , Label "vice versa" []
        ]
      pure l
    _ -> WitchHauntedWoodsTheLonelyTree <$> runMessage msg attrs
