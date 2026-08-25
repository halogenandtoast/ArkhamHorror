module Arkham.Homebrew.CircusExMortis.Acts.SmokeAndMirrors (smokeAndMirrors) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers (hasSealedMoonToken, releaseAMoonToken)
import Arkham.Matcher

newtype SmokeAndMirrors = SmokeAndMirrors ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smokeAndMirrors :: ActCard SmokeAndMirrors
smokeAndMirrors =
  act (2, A) SmokeAndMirrors Cards.smokeAndMirrors Nothing

instance HasAbilities SmokeAndMirrors where
  getAbilities (SmokeAndMirrors x) =
    [ playerLimit PerRound
        $ restricted x 1 (youExist hasSealedMoonToken)
        $ actionAbilityWithCost (HandDiscardCost 1 #any)
    , onlyOnce
        $ restricted
          x
          2
          (exists $ assetIs Assets.illusoryLocus <> AssetWithClues (AtLeast $ PerPlayer 2))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SmokeAndMirrors where
  runMessage msg a@(SmokeAndMirrors attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      releaseAMoonToken iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (assetIs Assets.illusoryLocus) removeFromGame
      -- the exit "reappears"; act 3 can only be met by resigning there
      reveal =<< placeSetAsideLocation Locations.circusGatesPathToFreedom
      advanceActDeck attrs
      pure a
    _ -> SmokeAndMirrors <$> liftRunMessage msg attrs
