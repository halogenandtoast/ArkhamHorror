module Arkham.Act.Cards.SmokeAndMirrorsCircusExMortis (smokeAndMirrorsCircusExMortis) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.CircusExMortis.Helpers (getSealedMoonTokens, releaseMoonToken)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SmokeAndMirrorsCircusExMortis = SmokeAndMirrorsCircusExMortis ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smokeAndMirrorsCircusExMortis :: ActCard SmokeAndMirrorsCircusExMortis
smokeAndMirrorsCircusExMortis =
  act (2, A) SmokeAndMirrorsCircusExMortis Cards.smokeAndMirrorsCircusExMortis Nothing

instance HasAbilities SmokeAndMirrorsCircusExMortis where
  getAbilities (SmokeAndMirrorsCircusExMortis x) =
    [ playerLimit PerRound
        $ mkAbility x 1
        $ actionAbilityWithCost (HandDiscardCost 1 #any)
    , restricted
        x
        2
        (exists $ assetIs Assets.illusoryLocusCircusExMortis <> AssetWithClues (AtLeast $ PerPlayer 2))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SmokeAndMirrorsCircusExMortis where
  runMessage msg a@(SmokeAndMirrorsCircusExMortis attrs) = runQueueT $ case msg of
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
      selectEach (assetIs Assets.illusoryLocusCircusExMortis) removeFromGame
      placeSetAsideLocation_ Locations.circusGatesPathToFreedomCircusExMortis
      advanceActDeck attrs
      pure a
    _ -> SmokeAndMirrorsCircusExMortis <$> liftRunMessage msg attrs
