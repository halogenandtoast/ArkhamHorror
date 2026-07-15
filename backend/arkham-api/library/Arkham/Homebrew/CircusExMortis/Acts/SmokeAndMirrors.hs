module Arkham.Homebrew.CircusExMortis.Acts.SmokeAndMirrors (smokeAndMirrors) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens, releaseMoonToken)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SmokeAndMirrors = SmokeAndMirrors ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smokeAndMirrors :: ActCard SmokeAndMirrors
smokeAndMirrors =
  act (2, A) SmokeAndMirrors Cards.smokeAndMirrors Nothing

instance HasAbilities SmokeAndMirrors where
  getAbilities (SmokeAndMirrors x) =
    [ playerLimit PerRound
        $ mkAbility x 1
        $ actionAbilityWithCost (HandDiscardCost 1 #any)
    , restricted
        x
        2
        (exists $ assetIs Assets.illusoryLocus <> AssetWithClues (AtLeast $ PerPlayer 2))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SmokeAndMirrors where
  runMessage msg a@(SmokeAndMirrors attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moons <- getSealedMoonTokens iid
      chooseOneM iid do
        for_ moons \token ->
          targeting (ChaosTokenTarget token) $ releaseMoonToken token
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (assetIs Assets.illusoryLocus) removeFromGame
      placeSetAsideLocation_ Locations.circusGatesPathToFreedom
      advanceActDeck attrs
      pure a
    _ -> SmokeAndMirrors <$> liftRunMessage msg attrs
