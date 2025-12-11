module Arkham.Act.Cards.TraversingTheOutside (traversingTheOutside) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (RevealLocation)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Effect.Window
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype TraversingTheOutside = TraversingTheOutside ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

traversingTheOutside :: ActCard TraversingTheOutside
traversingTheOutside = act (1, A) TraversingTheOutside Cards.traversingTheOutside Nothing

instance HasAbilities TraversingTheOutside where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1
      $ forced
      $ RevealLocation #when Anyone (locationIs Locations.outsidersLairWithoutATrace)

instance RunMessage TraversingTheOutside where
  runMessage msg a@(TraversingTheOutside attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      outsidersLair <- selectJust $ locationIs Locations.outsidersLairWithoutATrace
      createSetAsideEnemyWith_ Enemies.mimeticNemesisOtherworldlySubjugator outsidersLair createExhausted
      eachInvestigator \iid -> do
        discards <- select $ inDiscardOf iid
        focusCards discards do
          chooseNM iid 2 $ targets discards $ hollow iid
      theRedGlovedMan <- fetchCard Assets.theRedGlovedManHeWasAlwaysThere
      setCardAside theRedGlovedMan
      eachInvestigator \iid ->
        createWindowModifierEffect_
          (EffectHollowWindow theRedGlovedMan.id)
          ScenarioSource
          iid
          [Hollow theRedGlovedMan.id]
      createWindowModifierEffect_
        (EffectHollowWindow theRedGlovedMan.id)
        ScenarioSource
        theRedGlovedMan
        [CampaignModifier "hollowed"]
      advanceActDeck attrs
      pure a
    _ -> TraversingTheOutside <$> liftRunMessage msg attrs
