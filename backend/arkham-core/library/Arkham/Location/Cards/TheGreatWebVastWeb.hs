module Arkham.Location.Cards.TheGreatWebVastWeb (
  theGreatWebVastWeb,
  TheGreatWebVastWeb (..),
)
where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Movement

newtype TheGreatWebVastWeb = TheGreatWebVastWeb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebVastWeb :: LocationCard TheGreatWebVastWeb
theGreatWebVastWeb =
  locationWith
    TheGreatWebVastWeb
    Cards.theGreatWebVastWeb
    3
    (PerPlayer 2)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebVastWeb where
  getAbilities (TheGreatWebVastWeb attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          (Here <> not_ (OnAct 1) <> CluesOnThis (GreaterThanOrEqualTo (PerPlayer 1)))
          $ ActionAbility [#move]
          $ ActionCost 1
      ]

instance RunMessage TheGreatWebVastWeb where
  runMessage msg l@(TheGreatWebVastWeb attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let
        acrossLabel =
          case attrs.label of
            "theGreatWeb1" -> "theGreatWeb5"
            "theGreatWeb2" -> "theGreatWeb6"
            "theGreatWeb3" -> "theGreatWeb7"
            "theGreatWeb4" -> "theGreatWeb8"
            "theGreatWeb5" -> "theGreatWeb1"
            "theGreatWeb6" -> "theGreatWeb2"
            "theGreatWeb7" -> "theGreatWeb3"
            "theGreatWeb8" -> "theGreatWeb4"
            _ -> error "invalid label"
      acrossLocation <- selectJust $ LocationWithLabel acrossLabel
      push $ Move $ move (attrs.ability 1) iid acrossLocation
      pure l
    _ -> TheGreatWebVastWeb <$> lift (runMessage msg attrs)
