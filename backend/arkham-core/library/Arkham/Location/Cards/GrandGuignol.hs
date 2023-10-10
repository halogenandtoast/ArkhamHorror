module Arkham.Location.Cards.GrandGuignol (
  grandGuignol,
  GrandGuignol (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype GrandGuignol = GrandGuignol LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandGuignol :: LocationCard GrandGuignol
grandGuignol = location GrandGuignol Cards.grandGuignol 3 (PerPlayer 1)

instance HasAbilities GrandGuignol where
  getAbilities (GrandGuignol attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 Here
            $ ForcedAbility
            $ RevealLocation Timing.After You (LocationWithId $ toId attrs)
        ]

instance RunMessage GrandGuignol where
  runMessage msg a@(GrandGuignol attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      nonWeaknessCards <-
        selectListMap toCardId (BasicCardMatch NonWeakness <> InHandOf (InvestigatorWithId iid))
      drawing <- drawCards iid (toAbilitySource attrs 1) (length nonWeaknessCards)
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label "Take 2 Horror" [assignHorror iid (toAbilitySource attrs 1) 2]
        : [ Label
            "Shuffle all non-weakness cards from your hand into your deck, then draw an equal number of cards"
            (map (DiscardCard iid (toAbilitySource attrs 1)) nonWeaknessCards <> [drawing])
          | not (null nonWeaknessCards)
          ]
      pure a
    _ -> GrandGuignol <$> runMessage msg attrs
