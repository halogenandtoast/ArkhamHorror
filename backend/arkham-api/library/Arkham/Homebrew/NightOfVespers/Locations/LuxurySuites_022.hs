module Arkham.Homebrew.NightOfVespers.Locations.LuxurySuites_022 (luxurySuites_022) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (getSkillTestMatchingSkillIcons)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype LuxurySuites_022 = LuxurySuites_022 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luxurySuites_022 :: LocationCard LuxurySuites_022
luxurySuites_022 = symbolLabel $ location LuxurySuites_022 Cards.luxurySuites_022 2 (PerPlayer 2)

-- The reveal effect lasts the round, so it is held in meta and cleared at the
-- end of the round rather than read back off this location's own modifiers.
instance HasModifiersFor LuxurySuites_022 where
  getModifiersFor (LuxurySuites_022 a) = when (getLocationMetaDefault False a) do
    kinds <- getSkillTestMatchingSkillIcons
    when (any (`member` kinds) [#intellect, #willpower])
      $ modifySelect a Anyone [Difficulty 2]

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities LuxurySuites_022 where
  getAbilities (LuxurySuites_022 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [ mkAbility a 2 $ forced $ RevealLocation #when You (be a)
               , playerLimit PerRound
                   $ restricted
                     a
                     3
                     (exists $ TreacheryInThreatAreaOf You <> TreacheryIsNonWeakness)
                   $ FastAbility (ResourceCost 3)
               ]
        )

instance RunMessage LuxurySuites_022 where
  runMessage msg l@(LuxurySuites_022 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.conservatory
          , Cards.hotelRooftop
          ]
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure . LuxurySuites_022 $ attrs & setMeta True
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      treacheries <- select $ treacheryInThreatAreaOf iid <> TreacheryIsNonWeakness
      chooseTargetM iid treacheries $ toDiscardBy iid (attrs.ability 3)
      pure l
    EndRound -> do
      LuxurySuites_022 . setMeta @Bool False <$> liftRunMessage msg attrs
    _ -> LuxurySuites_022 <$> liftRunMessage msg attrs
