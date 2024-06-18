module Arkham.Location.Cards.GrandGuignol (grandGuignol, GrandGuignol (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype GrandGuignol = GrandGuignol LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandGuignol :: LocationCard GrandGuignol
grandGuignol = location GrandGuignol Cards.grandGuignol 3 (PerPlayer 1)

instance HasAbilities GrandGuignol where
  getAbilities (GrandGuignol attrs) =
    withRevealedAbilities
      attrs
      [restrictedAbility attrs 1 Here $ forced $ RevealLocation #after You (be attrs)]

instance RunMessage GrandGuignol where
  runMessage msg a@(GrandGuignol attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      nonWeaknessCards <- selectMap toCardId $ basic NonWeakness <> inHandOf iid
      let drawing = drawCards iid (attrs.ability 1) (length nonWeaknessCards)
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label "Take 2 Horror" [assignHorror iid (attrs.ability 1) 2]
        : [ Label
            "Shuffle all non-weakness cards from your hand into your deck, then draw an equal number of cards"
            (map (DiscardCard iid (attrs.ability 1)) nonWeaknessCards <> [drawing])
          | not (null nonWeaknessCards)
          ]
      pure a
    _ -> GrandGuignol <$> runMessage msg attrs
