module Arkham.Location.Cards.CandlelitTunnels (candlelitTunnels, CandlelitTunnels (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype CandlelitTunnels = CandlelitTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

candlelitTunnels :: LocationCard CandlelitTunnels
candlelitTunnels =
  locationWith CandlelitTunnels Cards.candlelitTunnels 3 (PerPlayer 2)
    $ (connectsToL .~ adjacentLocations)
    . ( costToEnterUnrevealedL
          .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
      )

instance HasAbilities CandlelitTunnels where
  getAbilities (CandlelitTunnels attrs) =
    extendRevealed
      attrs
      [ skillTestAbility $ groupLimit PerGame $ restrictedAbility attrs 1 Here actionAbility
      , restrictedAbility
          attrs
          2
          (oneOf [notExists $ LocationInDirection dir (be attrs) | dir <- [LeftOf, RightOf]])
          $ forced
          $ RevealLocation #when Anyone (be attrs)
      ]

instance RunMessage CandlelitTunnels where
  runMessage msg l@(CandlelitTunnels attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isAbilitySource attrs 1 source -> do
      player <- getPlayer iid
      locations <- select UnrevealedLocation
      unless (null locations)
        $ push
        $ chooseOne
          player
          [ targetLabel lid [LookAtRevealed iid source (LocationTarget lid)]
          | lid <- locations
          ]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      n <- countM (directionEmpty attrs) [LeftOf, RightOf]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [LeftOf, RightOf]
      pure l
    _ -> CandlelitTunnels <$> runMessage msg attrs
