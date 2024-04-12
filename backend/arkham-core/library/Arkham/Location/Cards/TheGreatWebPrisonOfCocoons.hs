module Arkham.Location.Cards.TheGreatWebPrisonOfCocoons (
  theGreatWebPrisonOfCocoons,
  TheGreatWebPrisonOfCocoons (..),
)
where

import Arkham.Ability
import Arkham.Direction
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype TheGreatWebPrisonOfCocoons = TheGreatWebPrisonOfCocoons LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebPrisonOfCocoons :: LocationCard TheGreatWebPrisonOfCocoons
theGreatWebPrisonOfCocoons =
  locationWith
    TheGreatWebPrisonOfCocoons
    Cards.theGreatWebPrisonOfCocoons
    4
    (PerPlayer 1)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebPrisonOfCocoons where
  getAbilities (TheGreatWebPrisonOfCocoons attrs) =
    extendRevealed attrs [forcedAbility attrs 1 $ Enters #after You $ be attrs]

instance RunMessage TheGreatWebPrisonOfCocoons where
  runMessage msg l@(TheGreatWebPrisonOfCocoons attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      beginSkillTest iid (attrs.ability 1) iid #agility (Fixed 3)
      pure l
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      actions <- field InvestigatorRemainingActions iid
      chooseOrRunOne
        iid
        $ [ Label ("Lose " <> pluralize n "action") [LoseActions iid (attrs.ability 1) n]
          | actions > 0
          ]
        <> [ Label "Place 1 doom on this location" [PlaceDoom (attrs.ability 1) (toTarget attrs) 1]
           ]

      pure l
    _ -> TheGreatWebPrisonOfCocoons <$> lift (runMessage msg attrs)
