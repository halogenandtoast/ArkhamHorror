module Arkham.Homebrew.NightOfVespers.Locations.MezzanineElevators_023 (mezzanineElevators_023) where

import Arkham.Ability
import Arkham.ChaosToken (ChaosTokenFace (..))
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MezzanineElevators_023 = MezzanineElevators_023 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mezzanineElevators_023 :: LocationCard MezzanineElevators_023
mezzanineElevators_023 =
  symbolLabel $ location MezzanineElevators_023 Cards.mezzanineElevators_023 3 (PerPlayer 1)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities MezzanineElevators_023 where
  getAbilities (MezzanineElevators_023 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage MezzanineElevators_023 where
  runMessage msg l@(MezzanineElevators_023 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.luxurySuites_021
          , Cards.luxurySuites_022
          , Cards.presidentialSuite_027
          , Cards.presidentialSuite_028
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      requestChaosTokens iid (attrs.ability 2) 1
      pure l
    RequestedChaosTokens (isAbilitySource attrs 2 -> True) (Just iid) tokens -> do
      continue_ iid
      faces <- getModifiedChaosTokenFaces tokens
      if any (`elem` [ElderSign, PlusOne, Zero, BlessToken]) faces
        then chooseOneM iid $ withI18n do
          labeled "takeAdditionalAction" $ takeActionAsIfTurn iid (attrs.ability 2)
          labeled "passOnAdditionalAction" nothing
        else assignDamage iid (attrs.ability 2) 1
      pure l
    _ -> MezzanineElevators_023 <$> liftRunMessage msg attrs
