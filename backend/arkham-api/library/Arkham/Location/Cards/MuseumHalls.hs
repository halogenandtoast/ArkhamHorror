module Arkham.Location.Cards.MuseumHalls (museumHalls, MuseumHalls (..)) where

import Arkham.Ability
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (museumHalls)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype MuseumHalls = MuseumHalls LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

museumHalls :: LocationCard MuseumHalls
museumHalls =
  locationWith MuseumHalls Cards.museumHalls 2 (Static 0)
    $ revealedConnectedMatchersL
    <>~ ["Exhibit Hall"]

instance HasModifiersFor MuseumHalls where
  getModifiersFor (MuseumHalls l) = whenUnrevealed l $ modifySelf l [Blocked]

instance HasAbilities MuseumHalls where
  getAbilities (MuseumHalls attrs) | attrs.unrevealed = do
    extend1
      attrs
      $ withTooltip
        "{action}: Test {combat} (5) to attempt to break down the door to the museum. If you are successful, immediately advance to Act 1b"
      $ restricted
        (proxied (LocationMatcherSource "Museum Entrance") attrs)
        1
        (OnLocation "Museum Entrance")
        #action
  getAbilities (MuseumHalls attrs) =
    extend1 attrs
      $ restricted attrs 1 Here
      $ actionAbilityWithCost
      $ GroupClueCost (PerPlayer 1) "Museum Halls"

instance RunMessage MuseumHalls where
  runMessage msg l@(MuseumHalls attrs) = runQueueT $ case msg of
    UseThisAbility iid this@(isProxySource attrs -> True) 1 | attrs.unrevealed -> do
      museumEntrance <- selectJust $ LocationWithTitle "Museum Entrance"
      sid <- getRandom
      beginSkillTest sid iid (toAbilitySource this 1) museumEntrance #combat (Fixed 5)
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 | attrs.revealed -> do
      push $ DrawCards iid $ targetCardDraw attrs ExhibitDeck 1
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      traverse_ placeLocation_ drewCards.cards
      pure l
    PassedThisSkillTest _ source@(isProxyAbilitySource attrs 1 -> True) -> do
      actId <- selectJust AnyAct
      push $ AdvanceAct actId source #other
      pure l
    _ -> MuseumHalls <$> liftRunMessage msg attrs
