module Arkham.Location.Cards.TheGreatWebCosmicWeb (
  theGreatWebCosmicWeb,
  TheGreatWebCosmicWeb (..),
)
where

import Arkham.Ability
import Arkham.Direction
import Arkham.Helpers.Message.Discard
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (toMessage)

newtype TheGreatWebCosmicWeb = TheGreatWebCosmicWeb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebCosmicWeb :: LocationCard TheGreatWebCosmicWeb
theGreatWebCosmicWeb =
  locationWith
    TheGreatWebCosmicWeb
    Cards.theGreatWebCosmicWeb
    4
    (PerPlayer 1)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebCosmicWeb where
  getAbilities (TheGreatWebCosmicWeb attrs) =
    extendRevealed attrs [skillTestAbility $ forcedAbility attrs 1 $ Enters #after You $ be attrs]

instance RunMessage TheGreatWebCosmicWeb where
  runMessage msg l@(TheGreatWebCosmicWeb attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      canDiscard <- iid <=~> InvestigatorWithDiscardableCard
      chooseOrRunOne
        iid
        $ [ Label ("Discard " <> pluralize n "card") [toMessage $ chooseAndDiscardCard iid (attrs.ability 1)]
          | canDiscard
          ]
        <> [ Label "Place 1 doom on this location" [PlaceDoom (attrs.ability 1) (toTarget attrs) 1]
           ]

      pure l
    _ -> TheGreatWebCosmicWeb <$> liftRunMessage msg attrs
