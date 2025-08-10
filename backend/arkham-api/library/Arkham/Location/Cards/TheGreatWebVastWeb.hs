module Arkham.Location.Cards.TheGreatWebVastWeb (theGreatWebVastWeb) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move

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
  getAbilities (TheGreatWebVastWeb a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> not_ (OnAct 1) <> not_ (CluesOnThis $ GreaterThanOrEqualTo (PerPlayer 1)))
      $ ActionAbility [#move] (ActionCost 1)

instance RunMessage TheGreatWebVastWeb where
  runMessage msg l@(TheGreatWebVastWeb attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let
        acrossLabel =
          case attrs.label of
            "theGreatWeb4" -> "theGreatWeb8"
            "theGreatWeb5" -> "theGreatWeb9"
            "theGreatWeb6" -> "theGreatWeb10"
            "theGreatWeb7" -> "theGreatWeb11"
            "theGreatWeb8" -> "theGreatWeb4"
            "theGreatWeb9" -> "theGreatWeb5"
            "theGreatWeb10" -> "theGreatWeb6"
            "theGreatWeb11" -> "theGreatWeb7"
            _ -> error "invalid label"
      acrossLocation <- selectJust $ LocationWithLabel acrossLabel
      moveTo (attrs.ability 1) iid acrossLocation
      pure l
    _ -> TheGreatWebVastWeb <$> liftRunMessage msg attrs
